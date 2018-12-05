extern crate airspy_sys as ffi;

use std::ffi::CStr;
use std::os::raw::{c_int, c_void};

use std::sync::mpsc::{SyncSender, Receiver, TrySendError, sync_channel};
use std::sync::atomic::{AtomicUsize, ATOMIC_USIZE_INIT, Ordering};

use std::{mem, slice, fmt, error};
use std::rc::Rc;

use ffi::airspy_error::*;

/// The maximum size of each of the buffers used for receiving.
/// Note: this must be greater than or equal to the buffer size chosen for libusb.
const BUFFER_SIZE: usize = 262144;

/// An internal macro for handling airspy errors.
macro_rules! airspy_try {
    ($e:expr) => ({
        if $e != AIRSPY_SUCCESS {
            return Err(parse_error($e));
        }
    });
}

/// A error that can occur when trying to interact with a Airspy device.
#[derive(Debug, Clone)]
pub struct AirspyError {
    /// The description of the code.
    pub desc: String,

    /// The code returned by `libairspy`.
    pub code: i32,
}

impl fmt::Display for AirspyError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Error({}): {}", self.code, self.desc)
    }
}

impl error::Error for AirspyError {
    fn description(&self) -> &str {
        &self.desc
    }
}

/// A specialized Result type for Airspy operations
pub type AirspyResult<T> = Result<T, AirspyError>;

/// An internal function for obtaining the description from a airspy error code.
fn parse_error(error_code: ffi::airspy_error) -> AirspyError {
    unsafe {
        let desc_ptr = ffi::airspy_error_name(error_code);
        let desc = CStr::from_ptr(desc_ptr).to_string_lossy();
        AirspyError {
            desc: desc.to_string(),
            code: error_code as i32,
        }
    }
}

/// Adjust a gain value to the nearest valid step
fn adjust_gain(gain: u8, min: u8, max: u8, step: u8) -> u8 {
    if gain < min { min }
    else if gain > max { max }
    else { (gain as f32 / step as f32).round() as u8 * step }
}

/// The Airspy context.
///
/// This struct is used ensure that `libairspy` is initialized before it can be used. A context can
/// be obtained by calling airspy::init().
pub struct AirspyContext(Rc<AirspyContextInner>);

/// Value for keeping track of any object that contains a reference to the context.
///
/// When this value is dropped, `libairspy` will be closed.
struct AirspyContextInner;

impl Drop for AirspyContextInner {
    fn drop(&mut self) {
        unsafe {
            ffi::airspy_exit();
        }
    }
}

/// Initialize `libairspy`, returning a context that can be used to connect to Airspy devices.
/// Once all references to the returned context go out of scope, the library will be closed
/// automatically.
pub fn init() -> AirspyResult<AirspyContext> {
    unsafe {
        airspy_try!(ffi::airspy_init());
    }
    Ok(AirspyContext(Rc::new(AirspyContextInner)))
}

/// A structure for managaing an Airspy device.
///
/// This structure contains methods for configuring the various settings. It also provides a method
/// for managing received samples as a blocking stream.
pub struct Airspy {
    inner: *mut ffi::airspy_device,
    _context: Rc<AirspyContextInner>,
}

impl Drop for Airspy {
    fn drop(&mut self) {
        unsafe {
            ffi::airspy_close(self.inner);
        }
    }
}

static OVERFLOW_COUNT: AtomicUsize = ATOMIC_USIZE_INIT;

impl Airspy {
    /// Attempt to open a connected Airspy device.
    pub fn open(context: &AirspyContext) -> AirspyResult<Airspy> {
        // Reset the overflow counter
        OVERFLOW_COUNT.store(0, Ordering::Relaxed);

        unsafe {
            let mut device = std::mem::zeroed();
            airspy_try!(ffi::airspy_open(&mut device));
            Ok(Airspy { inner: device, _context: context.0.clone() })
        }
    }

    /// Sets the center frequency (in Hz) of the Airspy: 24000000(24MHz) and 1750000000(1.75GHz).
    pub fn set_freq(&mut self, freq: u32) -> AirspyResult<()> {
        // TODO: Consider checking that the frequency is in a valid range.
        unsafe {
            airspy_try!(ffi::airspy_set_freq(self.inner, freq));
        }
        Ok(())
    }

    /// Set LNA gain: 0-15 dB
    /// If the specified gain falls outside of the valid range, the gain will be clamped.
    pub fn set_lna_gain(&mut self, gain: u8) -> AirspyResult<()> {
        unsafe {
            airspy_try!(ffi::airspy_set_lna_gain(self.inner, adjust_gain(gain, 0, 15, 1)));
        }
        Ok(())
    }

    /// Set mixer gain: 0-15 db
    /// If the specified gain falls outside of the valid range, the gain will be clamped.
    pub fn set_mixer_gain(&mut self, gain: u8) -> AirspyResult<()> {
        unsafe {
            airspy_try!(ffi::airspy_set_mixer_gain(self.inner, adjust_gain(gain, 0, 15, 1)));
        }
        Ok(())
    }

    /// Set VGA (IF) gain: 0-15 db
    /// If the specified gain falls outside of the valid range, the gain will be clamped.
    pub fn set_vga_gain(&mut self, gain: u8) -> AirspyResult<()> {
        unsafe {
            airspy_try!(ffi::airspy_set_vga_gain(self.inner, adjust_gain(gain, 0, 15, 1)));
        }
        Ok(())
    }

    /// Sets the sample rate in Hz
    pub fn set_samp_rate(&mut self, samp_rate: u32) -> AirspyResult<()> {
        unsafe {
            airspy_try!(ffi::airspy_set_samplerate(self.inner, samp_rate));

            // TODO: check if we need to apply a filter
            // airspy_try!(ffi::airspy_set_conversion_filter_float32(...));
        }
        Ok(())
    }

    /// Start an RX stream.
    pub fn rx_stream(&self, bound: usize) -> AirspyResult<RxStream> {
        let sample_type = ffi::airspy_sample_type::AIRSPY_SAMPLE_FLOAT32_IQ;
        unsafe {
            airspy_try!(ffi::airspy_set_sample_type(self.inner, sample_type));
        }

        let mut rx_stream = RxStream::new(bound, self);
        let sender = (&mut *rx_stream.sender) as *mut SyncSender<Vec<f32>>;
        unsafe {
            match ffi::airspy_start_rx(self.inner, Some(rx_callback), sender as *mut c_void) {
                AIRSPY_SUCCESS => Ok(rx_stream),
                error => Err(parse_error(error)),
            }
        }
    }

    /// Checks if the airspy is currently streaming
    pub fn is_streaming(&self) -> AirspyResult<()> {
        unsafe {
            match ffi::airspy_is_streaming(self.inner) {
                AIRSPY_TRUE => Ok(()),
                error => Err(parse_error(error)),
            }
        }
    }

    /// Return how many times the Airspy has dropped data frames
    pub fn overflow_count(&self) -> usize {
        OVERFLOW_COUNT.load(Ordering::Relaxed)
    }
}

/// A structure that manages receiving I/Q samples from the Airspy.
pub struct RxStream<'a> {
    airspy_device: &'a Airspy,
    local_index: usize,
    local_buffer: Vec<f32>,
    sender: Box<SyncSender<Vec<f32>>>,
    receiver: Receiver<Vec<f32>>,
    stopped: bool,
}

impl<'a> RxStream<'a> {
    /// Create a new instance of a RxStream
    fn new(bound: usize, airspy_device: &'a Airspy) -> RxStream<'a> {
        let (sender, receiver) = sync_channel(bound);
        RxStream {
            airspy_device: airspy_device,
            local_index: 0,
            local_buffer: vec![],
            sender: Box::new(sender),
            receiver: receiver,
            stopped: false,
        }
    }

    // Returns a reference to the internal receiver
    pub fn receiver(&mut self) -> &mut Receiver<Vec<f32>> {
        &mut self.receiver
    }

    /// Return the next I/Q sample converted to floating point numbers. If there is no data
    /// avaliable, then this function is blocking.
    #[inline]
    pub fn next_sample(&mut self) -> Option<(f32, f32)> {
        if self.local_buffer.len() < self.local_index + 1 {
            self.local_buffer = match self.receiver.recv() {
                Ok(buffer) => buffer,
                Err(_) => return None,
            };
            self.local_index = 0;
        }

        let i = self.local_index;
        self.local_index += 2;
        Some((self.local_buffer[i], self.local_buffer[i + 1]))
    }

    /// Stop the Rx Stream
    pub fn stop(mut self) -> AirspyResult<()> {
        unsafe {
            airspy_try!(ffi::airspy_stop_rx(self.airspy_device.inner));
        }
        self.stopped = true;
        Ok(())
    }
}

impl<'a> Drop for RxStream<'a> {
    fn drop(&mut self) {
        if !self.stopped {
            unsafe {
                ffi::airspy_stop_rx(self.airspy_device.inner);
            }
        }
    }
}

/// The call back that is used in the stream abstraction
unsafe extern "C" fn rx_callback(transfer: *mut ffi::airspy_transfer) -> c_int {
    if (*transfer).sample_type != ffi::airspy_sample_type::AIRSPY_SAMPLE_FLOAT32_IQ {
        return 1;
    }

    let src_buffer = slice::from_raw_parts(
        ((*transfer).samples) as *const f32,
        (*transfer).sample_count as usize * 2
    );

    // If the input buffer is too long, then an error has occured somewhere
    if src_buffer.len() > BUFFER_SIZE {
        return -1;
    }

    // Copy the transferred data into an owned structure
    let data = src_buffer.into();

    let sender_ptr: *mut SyncSender<Vec<f32>> = mem::transmute((*transfer).ctx);
    let sender = &mut *sender_ptr;

    if let Err(e) = sender.try_send(data) {
        match e {
            // If the buffer is full drop the message and increment the overflow count
            TrySendError::Full(_) => { OVERFLOW_COUNT.fetch_add(1, Ordering::Relaxed); },

            // Report an error back to libairspy
            TrySendError::Disconnected(_) => return -1,
        }
    }

    // Succesfully sent the data
    0
}

#[cfg(test)]
mod tests {
    #[test]
    fn init_test() {
        super::init().unwrap();
    }
}

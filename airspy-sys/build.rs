extern crate cc;
extern crate pkg_config;

fn main() {
    // Attempt to link to libairspy
    if let Err(_) = pkg_config::probe_library("airspy") {
        // Failed to link to system libairspy so attempt to build it instead.
        let files = &[
            "./libairspy/airspy.c",
            "./libairspy/iqconverter_float.c",
            "./libairspy/iqconverter_int16.c"
        ];
        cc::Build::new().files(files).compile("libairspy.a");

        // Attempt to link to libusb
        if let Err(_) = pkg_config::probe_library("libusb-1.0") {
            // For now just try to link to everything manually
            println!("cargo:rustc-link-lib=libusb-1.0");
            println!("cargo:rustc-link-lib=pthread")
        }
    }
}

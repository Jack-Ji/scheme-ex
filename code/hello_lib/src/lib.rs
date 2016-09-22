#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
    }
}

#[no_mangle]
pub extern fn print_hello() {
    println!("hello from lib!");
}

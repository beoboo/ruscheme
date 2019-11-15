use crate::error::Error;
use crate::byte_code::ByteCode;

#[derive(Debug)]
pub struct VirtualMachine {}

impl VirtualMachine {
    fn new() -> VirtualMachine {
        VirtualMachine {}
    }

    fn execute(&self, tokens: Vec<ByteCode>) -> Result<(), Error> {
        Err(Error::VirtualMachine(format!("Error")))
    }
}

#[cfg(test)]
mod tests {
    use hamcrest2::prelude::*;

    use super::*;

    #[test]
    fn exec_empty() {
        let vm = VirtualMachine::new();
        assert_that!(vm.execute(vec![]).is_err(), is(true));
    }
}
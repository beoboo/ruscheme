use crate::byte_code::{ByteCode, Instructions};
use crate::error::Error;

#[derive(Debug)]
pub struct VirtualMachine {}

impl VirtualMachine {
    pub(crate) fn new() -> VirtualMachine {
        VirtualMachine {}
    }

    pub(crate) fn execute(&self, instructions: Instructions) -> Result<(), Error> {
        if instructions.len() == 0 {
            return Err(Error::VirtualMachine(format!("No instructions to execute")));
        }

        self._execute(&instructions[0])
    }

    fn _execute(&self, instruction: &ByteCode) -> Result<(), Error> {
        match instruction {
            ByteCode::Constant(c) => {
                println!("{}", c);
            }
            _ => return Err(Error::VirtualMachine(format!("Undefined instruction: '{}'", instruction)))
        }

        Ok(())
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
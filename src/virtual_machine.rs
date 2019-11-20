use crate::byte_code::{ByteCode, Instructions};
use crate::error::{Error, report_stage_error};

#[derive(Debug)]
pub struct VirtualMachine {}

impl VirtualMachine {
    pub(crate) fn new() -> VirtualMachine {
        VirtualMachine {}
    }

    pub(crate) fn execute(&self, instructions: Instructions) -> Result<Instructions, Error> {
        let mut stack = vec![];
        if instructions.len() == 0 {
            return Err(Error::VirtualMachine(format!("No instructions to execute")));
        }

        let mut res = vec![];
        for i in instructions {
            res = self._execute(i, &mut stack)?;
        };

        Ok(res)
    }

    fn _execute(&self, instruction: ByteCode, stack: &mut Instructions) -> Result<Instructions, Error> {
        match instruction {
            ByteCode::Add => {
                let b = pop(stack)?;
                let a = pop(stack)?;

                let a = match a {
                    ByteCode::Constant(c) => c,
                    bc => return report_error(format!("Cannot add {}.", bc))
                };

                let b = match b {
                    ByteCode::Constant(c) => c,
                    bc => return report_error(format!("Cannot add {}.", bc))
                };

                push(stack, ByteCode::Constant(a + b))
            }
            ByteCode::Constant(_) => push(stack, instruction)
//            _ => Err(Error::VirtualMachine(format!("Undefined instruction: '{}'", instruction)))
        }?;

        Ok(stack.clone())
    }
}

fn pop(stack: &mut Instructions) -> Result<ByteCode, Error> {
    match stack.pop() {
        Some(b) => Ok(b),
        None => report_error("Stack underflow.")
    }
}

fn push(stack: &mut Instructions, instruction: ByteCode) -> Result<(), Error> {
    Ok(stack.push(instruction))
}

fn report_error<S: Into<String>, T>(err: S) -> Result<T, Error> {
    report_stage_error(err, "virtual_machine")
}


#[cfg(test)]
mod tests {
    use hamcrest2::prelude::*;

    use super::*;

    #[test]
    fn exec_empty() {
        assert_that!(execute(vec![]).is_err(), is(true));
    }

    #[test]
    fn exec_constant() {
        assert_valid(vec![ByteCode::Constant(1.0)], vec![ByteCode::Constant(1.0)]);
    }

    #[test]
    fn exec_add() {
        assert_valid(vec![
            ByteCode::Constant(1.0),
            ByteCode::Constant(2.0),
            ByteCode::Add,
        ], vec![
            ByteCode::Constant(3.0)
        ]);
        assert_valid(vec![
            ByteCode::Constant(1.0),
            ByteCode::Constant(2.0),
            ByteCode::Add,
            ByteCode::Constant(3.0),
            ByteCode::Add,
        ], vec![
            ByteCode::Constant(6.0)
        ]);
    }

    fn assert_valid(instructions: Instructions, expected: Instructions) {
        let res = execute(instructions).unwrap();

        assert_that!(res, equal_to(expected));
    }

    fn execute(instructions: Instructions) -> Result<Instructions, Error> {
        let vm = VirtualMachine::new();

        vm.execute(instructions)
    }
}
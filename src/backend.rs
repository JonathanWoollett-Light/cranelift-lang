use llvm_sys::analysis::LLVMVerifierFailureAction::LLVMAbortProcessAction;
use llvm_sys::bit_writer::LLVMWriteBitcodeToFile;
use llvm_sys::core::LLVMBuildRet;
use llvm_sys::execution_engine::LLVMGetFunctionAddress;
use llvm_sys::target::{LLVM_InitializeNativeAsmParser, LLVM_InitializeNativeAsmPrinter};
use llvm_sys::{
    analysis::LLVMVerifyModule,
    core::{
        LLVMAddFunction, LLVMAppendBasicBlock, LLVMBuildAdd, LLVMCreateBuilder,
        LLVMDisposeMessage, LLVMFunctionType, LLVMGetParam, LLVMInt32Type,
        LLVMModuleCreateWithName, LLVMPositionBuilderAtEnd,
    },
    execution_engine::{LLVMCreateExecutionEngineForModule, LLVMLinkInMCJIT},
    target::LLVM_InitializeNativeTarget,
};
use std::ffi::{c_int, CStr, CString};
use linked_syntax_tree::SyntaxTree;
use super::frontend::*;

use iced_x86::code_asm::*;

fn tree_gen(tree: SyntaxTree<Statement>) {
    // let asm = std::fs::OpenOptions::new().create(true).truncate(true).write(true).open("program.s");
    let mut assembler = ice_x86::code_asm::CodeAssembler::new(64);
    assembler.push(iced_x86::code_asm::rcx);
}

#[cfg(test)]
mod tests {
    use llvm_sys::analysis::LLVMVerifierFailureAction::LLVMAbortProcessAction;
    use llvm_sys::bit_writer::LLVMWriteBitcodeToFile;
    use llvm_sys::core::LLVMBuildRet;
    use llvm_sys::execution_engine::LLVMGetFunctionAddress;
    use llvm_sys::target::{LLVM_InitializeNativeAsmParser, LLVM_InitializeNativeAsmPrinter};
    use llvm_sys::{
        analysis::LLVMVerifyModule,
        core::{
            LLVMAddFunction, LLVMAppendBasicBlock, LLVMBuildAdd, LLVMCreateBuilder,
            LLVMDisposeMessage, LLVMFunctionType, LLVMGetParam, LLVMInt32Type,
            LLVMModuleCreateWithName, LLVMPositionBuilderAtEnd,
        },
        execution_engine::{LLVMCreateExecutionEngineForModule, LLVMLinkInMCJIT},
        target::LLVM_InitializeNativeTarget,
    };
    use std::ffi::{c_int, CStr, CString};

    // From https://www.pauladamsmith.com/blog/2015/01/how-to-get-started-with-llvm-c-api.html
    #[test]
    fn code_gen() {
        unsafe {
            let module_name = CString::new("my_module").unwrap();
            let module = LLVMModuleCreateWithName(module_name.as_c_str().as_ptr());

            let mut param_types = [LLVMInt32Type(), LLVMInt32Type()];
            let sum_return_type = LLVMFunctionType(LLVMInt32Type(), param_types.as_mut_ptr(), 2, 0);

            let function_name = CString::new("sum").unwrap();
            let sum = LLVMAddFunction(module, function_name.as_c_str().as_ptr(), sum_return_type);
            
            let temp = {

            };
            
            let block_name = CString::new("entry").unwrap();
            let entry = LLVMAppendBasicBlock(sum, block_name.as_c_str().as_ptr());

            let builder = LLVMCreateBuilder();
            LLVMPositionBuilderAtEnd(builder, entry);

            let result_name = CString::new("temp").unwrap();
            let temp = LLVMBuildAdd(
                builder,
                LLVMGetParam(sum, 0),
                LLVMGetParam(sum, 1),
                result_name.as_c_str().as_ptr(),
            );
            LLVMBuildRet(builder, temp);

            // Verify the module
            let mut error = std::ptr::null_mut();
            LLVMVerifyModule(module, LLVMAbortProcessAction, &mut error);
            LLVMDisposeMessage(error);

            // Run the module using a JIT execution engine
            let mut engine = std::ptr::null_mut();
            error = std::ptr::null_mut();

            LLVMLinkInMCJIT();
            // LLVMLinkInInterpreter();
            LLVM_InitializeNativeTarget();

            if LLVMCreateExecutionEngineForModule(&mut engine, module, &mut error) != 0 {
                println!("failed to create execution engine");
                return;
            }

            if !error.is_null() {
                println!("error: {}", CStr::from_ptr(error).to_str().unwrap());
                LLVMDisposeMessage(error);
                return;
            }

            // This was previously needed with `LLVMRunFunction`.
            // let mut args = [
            //     LLVMCreateGenericValueOfInt(LLVMInt32Type(), 1, 0),
            //     LLVMCreateGenericValueOfInt(LLVMInt32Type(), 2, 0),
            // ];

            // I don't know why these are needed, although they appear to resolve 1 of the issues.
            // From https://stackoverflow.com/a/38801376/4301453
            LLVM_InitializeNativeAsmPrinter();
            LLVM_InitializeNativeAsmParser();

            // I was experiencing issues with `LLVMRunFunction` looking this up I found
            // https://stackoverflow.com/a/63440756/4301453 which says to use `LLVMGetFunctionAddress`.
            let addr = LLVMGetFunctionAddress(engine, function_name.as_c_str().as_ptr());
            let f: extern "C" fn(c_int, c_int) -> c_int = std::mem::transmute(addr);
            let res = f(1i32, 2i32);
            println!("{}", res);
            assert_eq!(res, 3);

            let result_name = CString::new("sum.bc").unwrap();
            if LLVMWriteBitcodeToFile(module, result_name.as_c_str().as_ptr()) != 0 {
                println!("error writing bitcode to file");
            }

            // let res = LLVMRunFunction(engine, sum, 2, args.as_mut_ptr());
            // println!("{}", LLVMGenericValueToInt(res, 0));
        }
    }

    // #[test]
    // fn llvm_code() {
    //     unsafe {
    //         code_gen();
    //     }
    // }
}

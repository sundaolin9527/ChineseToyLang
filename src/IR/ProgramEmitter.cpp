#include "IR/ProgramEmitter.h"


//clang++-16 -std=c++17 -gdwarf-4 -O0 ir.cpp $(llvm-config-16 --cxxflags --ldflags --libs) -o ir
void EmitProgram(llvm::IRBuilder<>& Builder,
                llvm::LLVMContext& Context,
                llvm::Module* Module,
                const StmtSequence& program) {
    // 单次遍历处理所有语句
    for (StatementList* stmtNode = program.statements; stmtNode != nullptr; stmtNode = stmtNode->next) {
        if (!stmtNode->statement) continue;

        // 统一通过 EmitDecl 处理所有语句
        EmitDecl(Builder, Context, Module, stmtNode->statement);
    }

    // 验证模块完整性
    std::string verifyErrors;
    llvm::raw_string_ostream os(verifyErrors);
    if (llvm::verifyModule(*Module, &os)) {
        throw CodegenError("Module verification failed:\n" + os.str());
    }
    return
}

int main() {
    llvm::LLVMContext Context;
    llvm::Module Module("literal_demo", Context);
    llvm::IRBuilder<> Builder(Context);
    // ========================字面量测试===========================
    // 创建演示函数
    llvm::FunctionType* funcType = llvm::FunctionType::get(Builder.getVoidTy(), false);
    llvm::Function* func = llvm::Function::Create(
        funcType, llvm::Function::ExternalLinkage, "demo_literals", Module);

    llvm::BasicBlock* entry = llvm::BasicBlock::Create(Context, "entry", func);
    Builder.SetInsertPoint(entry);

    // 1. 整型字面量
    int8_t i8 = -42;
    llvm::Value* li8 = EmitLiteral(Builder, Context, &Module, TYPE_INT8, &i8);
    llvm::Value* i8_alloca = Builder.CreateAlloca(Builder.getInt8Ty(), nullptr, "i8.addr");
    Builder.CreateStore(li8, i8_alloca);

    int16_t i16 = -32768;
    llvm::Value* li16 = EmitLiteral(Builder, Context, &Module, TYPE_INT16, &i16);
    llvm::Value* i16_alloca = Builder.CreateAlloca(Builder.getInt16Ty(), nullptr, "i16.addr");
    Builder.CreateStore(li16, i16_alloca);

    int32_t i32 = 2147483647;
    llvm::Value* li32 = EmitLiteral(Builder, Context, &Module, TYPE_INT32, &i32);
    llvm::Value* i32_alloca = Builder.CreateAlloca(Builder.getInt32Ty(), nullptr, "i32.addr");
    Builder.CreateStore(li32, i32_alloca);

    int64_t i64 = -9223372036854775807LL;
    llvm::Value* li64 = EmitLiteral(Builder, Context, &Module, TYPE_INT64, &i64);
    llvm::Value* i64_alloca = Builder.CreateAlloca(Builder.getInt64Ty(), nullptr, "i64.addr");
    Builder.CreateStore(li64, i64_alloca);

    // 2. 无符号整型字面量
    uint8_t u8 = 255;
    llvm::Value* lu8 = EmitLiteral(Builder, Context, &Module, TYPE_UINT8, &u8);
    llvm::Value* u8_alloca = Builder.CreateAlloca(Builder.getInt8Ty(), nullptr, "u8.addr");
    Builder.CreateStore(lu8, u8_alloca);

    uint16_t u16 = 65535;
    llvm::Value* lu16 = EmitLiteral(Builder, Context, &Module, TYPE_UINT16, &u16);
    llvm::Value* u16_alloca = Builder.CreateAlloca(Builder.getInt16Ty(), nullptr, "u16.addr");
    Builder.CreateStore(lu16, u16_alloca);

    uint32_t u32 = 4294967295;
    llvm::Value* lu32 = EmitLiteral(Builder, Context, &Module, TYPE_UINT32, &u32);
    llvm::Value* u32_alloca = Builder.CreateAlloca(Builder.getInt32Ty(), nullptr, "u32.addr");
    Builder.CreateStore(lu32, u32_alloca);

    uint64_t u64 = 18446744073709551615ULL;
    llvm::Value* lu64 = EmitLiteral(Builder, Context, &Module, TYPE_UINT64, &u64);
    llvm::Value* u64_alloca = Builder.CreateAlloca(Builder.getInt64Ty(), nullptr, "u64.addr");
    Builder.CreateStore(lu64, u64_alloca);

    // 3. 浮点数字面量
    uint16_t f16_bits = 0x3C00; // 1.0 in half precision
    llvm::Value* lf16 = EmitLiteral(Builder, Context, &Module, TYPE_FLOAT16, &f16_bits);
    llvm::Value* f16_alloca = Builder.CreateAlloca(Builder.getHalfTy(), nullptr, "f16.addr");
    Builder.CreateStore(lf16, f16_alloca);

    float f32 = 3.1415926f;
    llvm::Value* lf32 = EmitLiteral(Builder, Context, &Module, TYPE_FLOAT32, &f32);
    llvm::Value* f32_alloca = Builder.CreateAlloca(Builder.getFloatTy(), nullptr, "f32.addr");
    Builder.CreateStore(lf32, f32_alloca);

    double f64 = 2.718281828459045;
    llvm::Value* lf64 = EmitLiteral(Builder, Context, &Module, TYPE_FLOAT64, &f64);
    llvm::Value* f64_alloca = Builder.CreateAlloca(Builder.getDoubleTy(), nullptr, "f64.addr");
    Builder.CreateStore(lf64, f64_alloca);

    // 4. 字符串字面量
    const char* str = "Hello LLVM! 你好！";
    llvm::Value* lstr = EmitLiteral(Builder, Context, &Module, TYPE_STRING, str);
    llvm::Value* str_alloca = Builder.CreateAlloca(Builder.getInt8PtrTy(), nullptr, "str.addr");
    Builder.CreateStore(lstr, str_alloca);

    // 5. 字符字面量
    char ch = 'A';
    llvm::Value* lch = EmitLiteral(Builder, Context, &Module, TYPE_CHAR, &ch);
    llvm::Value* ch_alloca = Builder.CreateAlloca(Builder.getInt8Ty(), nullptr, "ch.addr");
    Builder.CreateStore(lch, ch_alloca);

    // 6. 布尔字面量
    bool b = true;
    llvm::Value* lbool = EmitLiteral(Builder, Context, &Module, TYPE_BOOLEAN, &b);
    llvm::Value* bool_alloca = Builder.CreateAlloca(Builder.getInt1Ty(), nullptr, "bool.addr");
    Builder.CreateStore(lbool, bool_alloca);

    // 7. NULL字面量
    llvm::Value* lnull = EmitLiteral(Builder, Context, &Module, TYPE_PTR, nullptr);
    llvm::Value* null_alloca = Builder.CreateAlloca(Builder.getInt8PtrTy(), nullptr, "null.addr");
    Builder.CreateStore(lnull, null_alloca);

    // 函数返回
    Builder.CreateRetVoid();

    // 验证并打印模块
    if (llvm::verifyModule(Module, &llvm::errs())) {
    std::cerr << "Error verifying module!\n";
    return 1;
    }

    Module.print(llvm::outs(), nullptr);
    
    // ========================变量测试===========================
    llvm::LLVMContext Context1;
    llvm::Module Module1("var_decl_demo", Context1);
    llvm::IRBuilder<> Builder1(Context1);

    // 创建示例函数
    llvm::FunctionType* funcType1 = llvm::FunctionType::get(Builder1.getVoidTy(), false);
    llvm::Function* func1 = llvm::Function::Create(funcType1, llvm::Function::ExternalLinkage, "test_vars", Module1);
    llvm::BasicBlock* entry1 = llvm::BasicBlock::Create(Context1, "entry", func1);
    Builder1.SetInsertPoint(entry1);

    // 示例1：声明可修改的int32变量
    int32_t varValue1 = 42;
    VarDecl varDecl1 = {"myVar", VAR_TYPE_VARIABLE, TYPE_INT32, &varValue1};
    EmitVarDecl(Builder1, Context1, &Module1, varDecl1);

    // 示例2：声明浮点常量
    float constValue1 = 3.14f;
    VarDecl varDecl2 = {"PI", VAR_TYPE_CONSTANT, TYPE_FLOAT32, &constValue1};
    EmitVarDecl(Builder1, Context1, &Module1, varDecl2);

    // 示例3：声明字符串变量
    const char* strValue1 = "Hello";
    VarDecl varDecl3 = {"greeting", VAR_TYPE_VARIABLE, TYPE_STRING, strValue1};
    EmitVarDecl(Builder1, Context1, &Module1, varDecl3);

    Builder1.CreateRetVoid();

    // 验证并打印
    if (llvm::verifyModule(Module1, &llvm::errs())) {
        std::cerr << "Error verifying module!\n";
        return 1;
    }

    Module1.print(llvm::outs(), nullptr);

    // ========================函数测试===========================
    llvm::LLVMContext Context2;
    llvm::Module Module2("func_decl_demo", Context2);
    llvm::IRBuilder<> Builder2(Context2);

    // 示例1：声明 int foo(int a, float& b)
    FunctionDecl func21;
    func21.name = "foo";
    func21.returnType = TYPE_INT32;
    func21.params = {
        {"a", TYPE_INT32, false},
        {"b", TYPE_FLOAT32, true}
    };
    llvm::Function* fooFunc = EmitFunctionDecl(Builder2, Context2, &Module2, func21);

    // 示例2：声明 void bar(...)
    FunctionDecl func22;
    func22.name = "bar";
    func22.returnType = TYPE_VOID;
    func22.isVarArg = true;
    llvm::Function* barFunc = EmitFunctionDecl(Builder2, Context2, &Module2, func22);

    // 打印生成的IR
    Module2.print(llvm::outs(), nullptr);
    return 0;
}

//clang++-16 -std=c++17 -gdwarf-4 -O0 ir.cpp $(llvm-config-16 --cxxflags --ldflags --libs)  -o ir
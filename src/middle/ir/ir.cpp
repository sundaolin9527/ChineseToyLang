#include <iostream>
#include "ir.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/GlobalVariable.h"
#include "llvm/IR/Verifier.h"

//============================临时的，后续和ast集成需要使用ast.h中的定义====================
/* 变量声明类型 */
typedef enum {
    VAR_TYPE_VARIABLE, VAR_TYPE_CONSTANT
} VarType;

/* 类型 */
typedef enum ValueType{
    TYPE_UNKNOWN = 0,   // 未知类型
    TYPE_INT8,      // 1字节有符号整型
    TYPE_INT16,     // 2字节有符号整型
    TYPE_INT32,     // 4字节有符号整型
    TYPE_INT64,     // 8字节有符号整型
    TYPE_UINT8,      // 1字节无符号整型
    TYPE_UINT16,     // 2字节无符号整型
    TYPE_UINT32,     // 4字节无符号整型
    TYPE_UINT64,     // 8字节无符号整型
    TYPE_FLOAT16,   // 2字节浮点型
    TYPE_FLOAT32,   // 4字节浮点型
    TYPE_FLOAT64,   // 8字节浮点型
    TYPE_BOOLEAN,   // 布尔型
    TYPE_STRING,    // 字符串
    TYPE_CHAR,      // 字符型
    TYPE_VOID,      // 无类型
    TYPE_FUNCTION,  // 函数类型
    TYPE_ARRAY,     // 数组类型
    TYPE_STRUCT,    // 结构体
    TYPE_UNION,     // 联合体
    TYPE_PTR,       // 指针类型
    TYPE_ANY,        // 任意类型
} ValueType;
//=========================================

//clang++-16 -std=c++17 -gdwarf-4 -O0 ir.cpp $(llvm-config-16 --cxxflags --ldflags --libs) -o ir
llvm::Type* ConvertToLLVMType(llvm::LLVMContext& Context, 
                             llvm::IRBuilder<>& Builder,
                             ValueType type) {
    switch (type) {
        // 有符号整型
        case TYPE_INT8:    return Builder.getInt8Ty();
        case TYPE_INT16:   return Builder.getInt16Ty();
        case TYPE_INT32:   return Builder.getInt32Ty();
        case TYPE_INT64:   return Builder.getInt64Ty();
        
        // 无符号整型（LLVM用相同类型表示，区别在于操作指令）
        case TYPE_UINT8:  return Builder.getInt8Ty();
        case TYPE_UINT16: return Builder.getInt16Ty();
        case TYPE_UINT32: return Builder.getInt32Ty();
        case TYPE_UINT64: return Builder.getInt64Ty();
        
        // 浮点型
        case TYPE_FLOAT16: return Builder.getHalfTy();
        case TYPE_FLOAT32: return Builder.getFloatTy();
        case TYPE_FLOAT64: return Builder.getDoubleTy();
        
        // 基本类型
        case TYPE_BOOLEAN: return Builder.getInt1Ty();
        case TYPE_CHAR:   return Builder.getInt8Ty();
        case TYPE_STRING:  return Builder.getInt8PtrTy();
        case TYPE_VOID:   return Builder.getVoidTy();
        case TYPE_PTR:     return Builder.getPtrTy();
        
        // 复合类型需要额外处理
        case TYPE_ARRAY:   return nullptr; // 需要额外提供元素类型和长度
        case TYPE_STRUCT:  return nullptr; // 需要提供字段类型列表
        case TYPE_UNION:   return nullptr; // 类似结构体但内存布局不同
        case TYPE_FUNCTION: return nullptr; // 需要提供函数签名
        
        case TYPE_UNKNOWN:
        case TYPE_ANY:
        default:
            llvm::errs() << "Unknown or unsupported type\n";
            return nullptr;
    }
}

llvm::Type* ConvertArrayType(llvm::LLVMContext& Context,
                           llvm::Type* elementType,
                           uint64_t length) {
    if (!elementType) return nullptr;
    return llvm::ArrayType::get(elementType, length);
}

llvm::Type* ConvertStructType(llvm::LLVMContext& Context,
                             const std::vector<llvm::Type*>& fieldTypes,
                             const std::string& name = "",
                             bool isPacked = false) {
    return llvm::StructType::create(Context, fieldTypes, name, isPacked);
}

llvm::FunctionType* ConvertFunctionType(llvm::LLVMContext& Context,
                                      llvm::Type* returnType,
                                      const std::vector<llvm::Type*>& paramTypes,
                                      bool isVarArg = false) {
    return llvm::FunctionType::get(returnType, paramTypes, isVarArg);
}

llvm::Value* EmitLiteral(llvm::IRBuilder<> &Builder, llvm::LLVMContext &Context, llvm::Module *Module, ValueType type, const void *value) 
{
  // 如果未传入 Module，尝试从 Builder 获取
  if (!Module && Builder.GetInsertBlock()) {
    Module = Builder.GetInsertBlock()->getModule();
  }
  switch (type) {
    // 整数类型
    case TYPE_INT8:
      return llvm::ConstantInt::get(Context, llvm::APInt(8, *(int8_t*)value, true));
    case TYPE_INT16:
      return llvm::ConstantInt::get(Context, llvm::APInt(16, *(int16_t*)value, true));
    case TYPE_INT32:
      return llvm::ConstantInt::get(Context, llvm::APInt(32, *(int32_t*)value, true));
    case TYPE_INT64:
      return llvm::ConstantInt::get(Context, llvm::APInt(64, *(int64_t*)value, true));

    // 无符号整数
    case TYPE_UINT8:
      return llvm::ConstantInt::get(Context, llvm::APInt(8, *(uint8_t*)value, false));
    case TYPE_UINT16:
      return llvm::ConstantInt::get(Context, llvm::APInt(16, *(uint16_t*)value, false));
    case TYPE_UINT32:
      return llvm::ConstantInt::get(Context, llvm::APInt(32, *(uint32_t*)value, false));
    case TYPE_UINT64:
      return llvm::ConstantInt::get(Context, llvm::APInt(64, *(uint64_t*)value, false));

    // 浮点数
    case TYPE_FLOAT16:
      return llvm::ConstantFP::get(Context, llvm::APFloat(llvm::APFloat::IEEEhalf(), *(uint16_t*)value));
    case TYPE_FLOAT32:
      return llvm::ConstantFP::get(Context, llvm::APFloat(*(float*)value));
    case TYPE_FLOAT64:
      return llvm::ConstantFP::get(Context, llvm::APFloat(*(double*)value));

    // 字符串（全局常量）
    case TYPE_STRING: {
      const char *str = (const char*)value;
      llvm::Constant *StrConst = llvm::ConstantDataArray::getString(Context, str);
      
      if (!Module) {
        // 如果没有 Module，无法创建全局变量，返回 nullptr 或报错
        return nullptr;
      }

      llvm::GlobalVariable *GV = new llvm::GlobalVariable(
          *Module,
          StrConst->getType(),
          true,
          llvm::GlobalValue::PrivateLinkage,
          StrConst,
          "str.literal");

      if (Builder.GetInsertBlock()) {
        // 如果有插入点，生成 GEP 指令
        return Builder.CreateInBoundsGEP(
            GV->getValueType(),
            GV,
            {Builder.getInt32(0), Builder.getInt32(0)});
      } else {
        // 全局初始化，直接返回全局变量
        return GV;
      }
    }

    // 字符
    case TYPE_CHAR:
      return llvm::ConstantInt::get(Context, llvm::APInt(8, *(char*)value, false));

    // 布尔值
    case TYPE_BOOLEAN:
      return llvm::ConstantInt::get(Context, llvm::APInt(1, *(bool*)value, false));

    // NULL（指针）
    case TYPE_PTR:
      return llvm::ConstantPointerNull::get(Builder.getPtrTy());

    default:
      return nullptr;  // 其他
  }
}

// 变量声明结构
struct VarDecl {
    std::string name;      // 变量名
    VarType varType;       // 变量类型（VAR_TYPE_VARIABLE 或 VAR_TYPE_CONSTANT）
    ValueType litType;     // 字面量类型
    const void* initValue; // 初始值指针
};

llvm::Value* EmitVarDecl(llvm::IRBuilder<>& Builder, 
                        llvm::LLVMContext& Context,
                        llvm::Module* Module,
                        const VarDecl& decl) {
    
    // 1. 根据字面量类型获取LLVM类型, 先只讨论字面量
    llvm::Type* ty = ConvertToLLVMType(Context, Builder, decl.litType);
    if (ty == nullptr)
    {
        std::cerr << "Unsupported or complex type for variable: " << decl.litType << "\n";
        return nullptr;
    }

    // 2. 创建alloca指令（在栈上分配空间）
    llvm::Value* alloc = Builder.CreateAlloca(ty, nullptr, decl.name);

    // 3. 处理初始化值
    if (decl.initValue) {
        llvm::Value* initVal = EmitLiteral(Builder, Context, Module, decl.litType, decl.initValue);
        Builder.CreateStore(initVal, alloc);
    }

    // 4. 如果是常量，标记为不可修改
    if (decl.varType == VAR_TYPE_CONSTANT) {
        if (llvm::GlobalVariable* GV = llvm::dyn_cast<llvm::GlobalVariable>(alloc)) {
            GV->setConstant(true);
        }
    }

    return alloc;
}

// 函数参数信息结构
struct ParamDecl {
    std::string name;
    ValueType type;
    bool isReference; // 是否引用传递
};

// 函数声明结构
struct FunctionDecl {
    std::string name;
    ValueType returnType;
    std::vector<ParamDecl> params;
    bool isVarArg = false; // 是否可变参数
};

llvm::Function* EmitFunctionDecl(llvm::IRBuilder<>& Builder,
                                llvm::LLVMContext& Context,
                                llvm::Module* Module,
                                const FunctionDecl& funcDecl) {
    // 1. 转换返回类型
    llvm::Type* retTy = ConvertToLLVMType(Context, Builder, funcDecl.returnType);
    if (retTy == nullptr)
    {
        std::cerr << "Unsupported return type for function: " << funcDecl.name << "\n";
        return nullptr;
    }

    // 2. 转换参数类型
    std::vector<llvm::Type*> paramTypes;
    for (const auto& param : funcDecl.params) {
        llvm::Type* ty = ConvertToLLVMType(Context, Builder, param.type);
        if (ty == nullptr)
        {
            return nullptr;
        }
        
        if (param.isReference) {
            ty = ty->getPointerTo(); // 引用类型转换为指针
        }
        paramTypes.push_back(ty);
    }

    // 3. 创建函数类型
    llvm::FunctionType* funcType = llvm::FunctionType::get(
        retTy, paramTypes, funcDecl.isVarArg);

    // 4. 创建函数对象
    llvm::Function* func = llvm::Function::Create(
        funcType, llvm::Function::ExternalLinkage, funcDecl.name, Module);

    // 5. 设置参数名称
    unsigned idx = 0;
    for (auto& arg : func->args()) {
        arg.setName(funcDecl.params[idx++].name);
    }

    return func;
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
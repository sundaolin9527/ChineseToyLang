#ifndef CODEGEN_H
#define CODEGEN_H

#include <iostream>
#include "Basic/LinkedList.h"
#include "Frontend/Ast.h"
#include "Basic/Types.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/GlobalVariable.h"
#include "llvm/IR/Verifier.h"


// 前向声明
namespace llvm {
    class Value;
    class Type;
    class Function;
}

class CodeGenerator {
public:
    // 符号信息结构体
    struct SymbolInfo {
        llvm::Value* value; // LLVM IR值
        llvm::Type* type;   // LLVM类型
        bool isConst;       // 是否是常量
        
        // 构造函数
        SymbolInfo(Node n, llvm::Value* v, llvm::Type* t, bool isConst = false)
            : value(v), type(t), isConst(isConst) {}
    };

    /// @brief 构造函数
    explicit CodeGenerator(const std::string& moduleName);

    /// @brief 生成整个程序的LLVM IR
    /// @param program 程序根节点
    void EmitProgram(ASTNode* program);

    /// @brief 打印生成的IR
    void dumpIR() const;

private:
    // LLVM核心组件
    llvm::LLVMContext Context;
    llvm::Module Module;
    llvm::IRBuilder<> Builder;
    
    // 符号表相关, 和前端的符号表实现不一致，后面再优化
    std::vector<std::unordered_map<std::string, SymbolInfo>> symbolTables;
    
    /// @brief 进入新的作用域
    void EnterScope();
    
    /// @brief 离开当前作用域
    void ExitScope();
    
    /// @brief 添加符号到当前作用域
    /// @return 是否添加成功
    bool AddSymbol(const std::string& name, const SymbolInfo& info);
    
    /// @brief 查找符号
    SymbolInfo* LookupSymbol(const std::string& name);
    
    // 各种AST节点的生成方法
    llvm::Value* EmitLiteralExpr(ASTNode* expr) ;
    llvm::Value* EmitIdentifierExpr(ASTNode* expr);
    llvm::Value* EmitBinaryExpr(ASTNode* expr);
    llvm::Value* EmitAssignmentExpr(ASTNode* expr);
    llvm::Value* EmitCallExpr(ASTNode* expr);
    llvm::Value* EmitArrayAccessExpr(ASTNode* expr);
    llvm::Value* EmitObjectAccessExpr(ASTNode* expr);
    llvm::Value* EmitAnonymousFuncExpr(ASTNode* expr);
    llvm::Value* EmitExpr(ASTNode* expr);

    llvm::Value* EmitContinueStmt();
    llvm::Value* EmitBreakStmt();
    llvm::Value* EmitBlockStmt(StmtSequence& blockStmt, llvm::Function* currentFunction);
    llvm::Value* EmitExportStmt(Name& exportStmt, llvm::Function* currentFunction);
    llvm::Value* EmitImportStmt(Name& importStmt, llvm::Function* currentFunction);
    llvm::Value* EmitExprStmt(SimpleStmt& exprStmt, llvm::Function* currentFunction);
    llvm::Value* EmitWhileStmt(WhileStmt& whileStmt, llvm::Function* currentFunction);
    llvm::Value* EmitForStmt(ForStmt& forStmt, llvm::Function* currentFunction);
    llvm::Value* EmitIfStmt(IfStmt& ifStmt, llvm::Function* currentFunction);
    llvm::Value* EmitReturnStmt(SimpleStmt& returnStmt, llvm::Function* currentFunction);
    llvm::Value* EmitStmt(ASTNode* stmt, llvm::Function* currentFunction);

    llvm::Value* EmitVarDecl(ASTNode *node);
    llvm::Function* EmitFunctionDecl(ASTNode* funcDecl);
    llvm::Type* EmitStructOrUnionDecl(ASTNode *node);
    llvm::Value* EmitDecl(ASTNode* decl, llvm::Function* currentFunction = nullptr);

    // 检查相关
    llvm::Constant* ConstantFoldBinaryOp(Operator op, llvm::Constant* L, llvm::Constant* R, TypeKind type);
    void emitBoundsCheck(llvm::Value* array, llvm::Value* index, int line);
    llvm::Type* ConvertToLLVMType(TypeKind type);
    llvm::Type* ConvertArrayType(llvm::Type* elementType, uint64_t length);
    llvm::Type* ConvertStructType(const std::vector<llvm::Type*>& fieldTypes,
                                const std::string& name = "", bool isPacked = false);
    llvm::FunctionType* ConvertFunctionType(llvm::Type* returnType,
                                const std::vector<llvm::Type*>& paramTypes, bool isVarArg = false);
};

#endif /* CODEGEN_H */
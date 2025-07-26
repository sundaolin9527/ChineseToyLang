#include <iostream>
#include <vector>
#include <string>
#include <unordered_map>
#include <set>
#include <algorithm>
#include <memory>
#include <stdexcept>

// 虚拟指令类型
enum class IROp {
    ADD, SUB, MUL, DIV, 
    LOAD, STORE, 
    MOV, 
    CALL, RET,
    BRANCH, CMP
};

// 操作数类型
struct Operand {
    enum class Kind { VREG, PHYS_REG, MEM, IMMEDIATE };
    Kind kind;
    std::string name;  // 寄存器名或内存地址
    int value;         // 立即数值
    
    Operand(Kind k, const std::string& n = "", int v = 0) 
        : kind(k), name(n), value(v) {}
    
    bool isVReg() const { return kind == Kind::VREG; }
    bool isPhysReg() const { return kind == Kind::PHYS_REG; }
    bool isMem() const { return kind == Kind::MEM; }
    bool isImm() const { return kind == Kind::IMMEDIATE; }
    
    std::string toString() const {
        if (isVReg()) return "%" + name;
        if (isPhysReg()) return name;
        if (isMem()) return "[" + name + "]";
        return std::to_string(value);
    }
};

// 虚拟指令结构
struct IRInstruction {
    IROp op;
    Operand dest;
    Operand src1;
    Operand src2;
    
    IRInstruction(IROp o, Operand d, Operand s1, Operand s2 = Operand(Operand::Kind::IMMEDIATE, "", 0))
        : op(o), dest(d), src1(s1), src2(s2) {}
    
    std::string toString() const {
        std::string result;
        switch(op) {
            case IROp::ADD: result = "ADD"; break;
            case IROp::SUB: result = "SUB"; break;
            case IROp::MUL: result = "MUL"; break;
            case IROp::DIV: result = "DIV"; break;
            case IROp::LOAD: result = "LOAD"; break;
            case IROp::STORE: result = "STORE"; break;
            case IROp::MOV: result = "MOV"; break;
            case IROp::CALL: result = "CALL"; break;
            case IROp::RET: result = "RET"; break;
            case IROp::BRANCH: result = "BRANCH"; break;
            case IROp::CMP: result = "CMP"; break;
        }
        result += " " + dest.toString();
        if (src1.kind != Operand::Kind::IMMEDIATE || src1.value != 0)
            result += ", " + src1.toString();
        if (src2.kind != Operand::Kind::IMMEDIATE || src2.value != 0)
            result += ", " + src2.toString();
        return result;
    }
};

// 活跃区间
struct LiveInterval {
    std::string vreg;
    int start;
    int end;
    
    LiveInterval(const std::string& vr, int s, int e) 
        : vreg(vr), start(s), end(e) {}
    
    bool overlaps(const LiveInterval& other) const {
        return !(end <= other.start || other.end <= start);
    }
    
    bool operator<(const LiveInterval& other) const {
        return start < other.start || (start == other.start && end < other.end);
    }
};

class LinearScanAllocator {
    std::vector<std::string> physRegs;
    std::unordered_map<std::string, std::string> vregToPhysReg;
    std::unordered_map<std::string, int> spilledVars;
    int nextSpillOffset = 0;
    
public:
    LinearScanAllocator(const std::vector<std::string>& regs) 
        : physRegs(regs) {}
    
    void allocate(std::vector<LiveInterval>& intervals) {
        std::sort(intervals.begin(), intervals.end());
        std::vector<std::pair<std::string, int>> active;
        
        for (auto& interval : intervals) {
            expireOldIntervals(active, interval.start);
            
            if (physRegs.empty()) {
                spillAtInterval(active, interval);
            } else {
                assignRegister(interval);
                active.emplace_back(interval.vreg, interval.end);
                std::sort(active.begin(), active.end(), 
                    [](const auto& a, const auto& b) { return a.second < b.second; });
            }
        }
    }
    
    std::string getPhysReg(const std::string& vreg) const {
        auto it = vregToPhysReg.find(vreg);
        if (it != vregToPhysReg.end()) return it->second;
        
        auto spillIt = spilledVars.find(vreg);
        if (spillIt != spilledVars.end()) {
            return "[SP+" + std::to_string(spillIt->second) + "]";
        }
        
        return "";
    }

    const std::unordered_map<std::string, std::string>& getRegMap() const {
        return vregToPhysReg;
    }
    
private:
    void expireOldIntervals(std::vector<std::pair<std::string, int>>& active, int pos) {
        active.erase(std::remove_if(active.begin(), active.end(),
            [&, this](const auto& item) {
                if (item.second <= pos) {
                    physRegs.push_back(vregToPhysReg[item.first]);
                    vregToPhysReg.erase(item.first);
                    return true;
                }
                return false;
            }), active.end());
    }
    
    void spillAtInterval(std::vector<std::pair<std::string, int>>& active, 
                        const LiveInterval& interval) {
        auto spillCandidate = std::max_element(active.begin(), active.end(),
            [](const auto& a, const auto& b) { return a.second < b.second; });
        
        if (spillCandidate != active.end() && spillCandidate->second > interval.end) {
            spilledVars[spillCandidate->first] = nextSpillOffset;
            nextSpillOffset += 4;
            vregToPhysReg[interval.vreg] = vregToPhysReg[spillCandidate->first];
            active.erase(spillCandidate);
            active.emplace_back(interval.vreg, interval.end);
        } else {
            spilledVars[interval.vreg] = nextSpillOffset;
            nextSpillOffset += 4;
        }
    }
    
    void assignRegister(const LiveInterval& interval) {
        // 优先分配调用者保存的寄存器
        const std::vector<std::string> registerPriority = {
            "rax", "rcx", "rdx", "rsi", "rdi", "r8", "r9", "r10", "r11"  // 调用者保存
        };
        
        for (const auto& reg : registerPriority) {
            if (std::find(physRegs.begin(), physRegs.end(), reg) != physRegs.end()) {
                physRegs.erase(std::remove(physRegs.begin(), physRegs.end(), reg), physRegs.end());
                vregToPhysReg[interval.vreg] = reg;
                return;
            }
        }
        
        // 如果没有可用寄存器再使用被调用者保存的寄存器
        std::string reg = physRegs.back();
        physRegs.pop_back();
        vregToPhysReg[interval.vreg] = reg;
    }
};

class InstructionSelector {
public:
    struct MachineInstruction {
        std::string opcode;
        Operand dest;
        Operand src1;
        Operand src2;
        
        MachineInstruction();
        MachineInstruction(const std::string& op, 
                         const Operand& d = Operand(Operand::Kind::IMMEDIATE, "", 0),
                         const Operand& s1 = Operand(Operand::Kind::IMMEDIATE, "", 0),
                         const Operand& s2 = Operand(Operand::Kind::IMMEDIATE, "", 0))
            : opcode(op), dest(d), src1(s1), src2(s2) {}
        
        std::string toString() const {
            std::string result = opcode + " " + dest.toString();
            if (src1.kind != Operand::Kind::IMMEDIATE || src1.value != 0)
                result += ", " + src1.toString();
            if (src2.kind != Operand::Kind::IMMEDIATE || src2.value != 0)
                result += ", " + src2.toString();
            return result;
        }
    };
    
    virtual std::vector<MachineInstruction> select(
        const std::vector<IRInstruction>& ir,
        const std::unordered_map<std::string, std::string>& regMap) = 0;
    
    virtual ~InstructionSelector() = default;
};

class X86InstructionSelector : public InstructionSelector {
public:
    std::vector<MachineInstruction> select(
        const std::vector<IRInstruction>& ir,
        const std::unordered_map<std::string, std::string>& regMap) override {
        
        std::vector<MachineInstruction> machineCode;
        int spillOffset = 0;
        std::unordered_map<std::string, int> spillMap;
        
        auto mapOperand = [&](const Operand& op) {
            if (op.isVReg()) {
                if (auto it = regMap.find(op.name); it != regMap.end()) {
                    return Operand(Operand::Kind::PHYS_REG, it->second);
                }
                // 确保每个溢出变量有唯一偏移
                if (!spillMap.count(op.name)) {
                    spillMap[op.name] = spillOffset;
                    spillOffset += 4;
                }
                return Operand(Operand::Kind::MEM, "rsp+" + std::to_string(spillMap[op.name]));
            }
            return op;
        };

        // 优化立即数处理
        auto optimizeImmediate = [](const Operand& op, MachineInstruction& mi) {
            if (op.isImm()) {
                // 小立即数直接嵌入指令
                if (op.value >= -128 && op.value <= 127) {
                    mi.src1 = Operand(Operand::Kind::IMMEDIATE, "", op.value);
                    return true;
                }
            }
            return false;
        };
        
        for (const auto& inst : ir) {
            switch (inst.op) {
                case IROp::MOV:
                    if (inst.src1.isImm()) {
                        machineCode.emplace_back("mov", mapOperand(inst.dest), inst.src1);
                    } else {
                        machineCode.emplace_back("mov", mapOperand(inst.dest), mapOperand(inst.src1));
                    }
                    break;
                case IROp::ADD: {
                    auto dest = mapOperand(inst.dest);
                    auto src1 = mapOperand(inst.src1);
                    auto src2 = mapOperand(inst.src2);
                    
                    if (dest.toString() != src1.toString()) {
                        machineCode.emplace_back("mov", dest, src1);
                    }
                    machineCode.emplace_back("add", dest, src2);
                    break;
                }
                case IROp::SUB:
                    machineCode.emplace_back("sub", mapOperand(inst.dest), mapOperand(inst.src1));
                    break;
                case IROp::MUL:
                    machineCode.emplace_back("imul", mapOperand(inst.dest), mapOperand(inst.src1));
                    break;
                case IROp::LOAD: {
                    Operand dest = mapOperand(inst.dest);
                    Operand src = Operand(Operand::Kind::MEM, mapOperand(inst.src1).toString());
                    machineCode.emplace_back("mov", dest, src);
                    break;
                }
                case IROp::STORE:
                    machineCode.emplace_back("mov", 
                        Operand(Operand::Kind::MEM, mapOperand(inst.dest).name),
                        mapOperand(inst.src1));
                    break;
                case IROp::RET:
                    machineCode.emplace_back("ret");
                    break;
                default:
                    std::cerr << "Error: Unsupported IR operation\n";
            }
        }
        return machineCode;
    }
private:
    int nextSpillOffset = 0;  // 跟踪溢出变量的偏移量
};

// clang++-16 -std=c++17 -gdwarf-4 -O0 CodeGen.cpp $(llvm-config-16 --cxxflags --ldflags --libs) -o CodeGen
int main() {
    std::vector<IRInstruction> irCode = {
        {IROp::MOV, Operand(Operand::Kind::VREG, "a"), Operand(Operand::Kind::IMMEDIATE, "", 5)},
        {IROp::MOV, Operand(Operand::Kind::VREG, "b"), Operand(Operand::Kind::IMMEDIATE, "", 10)},
        {IROp::ADD, Operand(Operand::Kind::VREG, "c"), Operand(Operand::Kind::VREG, "a"), Operand(Operand::Kind::VREG, "b")},
        {IROp::STORE, Operand(Operand::Kind::MEM, "ptr"), Operand(Operand::Kind::VREG, "c")},
        {IROp::RET, Operand(Operand::Kind::IMMEDIATE, "", 0), Operand(Operand::Kind::IMMEDIATE, "", 0)}
    };
    
    std::vector<LiveInterval> intervals = {
        {"1", 1, 2},   // %1 只在第1-2条指令活跃
        {"2", 2, 3},   // %2 只在第2-3条指令活跃
        {"3", 3, 4},   // %3 只在第3-4条指令活跃
        {"ptr", 4, 5}  // ptr只在存储时活跃
    };
    
    LinearScanAllocator allocator({
        "rax", "rbx", "rcx", "rdi", // 64位寄存器
        "r8", "r9", "r10"           // 更多寄存器
    });
    allocator.allocate(intervals);
    
    std::cout << "Register Allocation:\n";
    for (const auto& interval : intervals) {
        std::cout << "%" << interval.vreg << " -> " << allocator.getPhysReg(interval.vreg) << "\n";
    }
    
    X86InstructionSelector selector;
    auto machineCode = selector.select(irCode, allocator.getRegMap());
    
    std::cout << "\nMachine Code:\n";
    for (const auto& inst : machineCode) {
        std::cout << inst.toString() << "\n";
    }
    
    return 0;
}
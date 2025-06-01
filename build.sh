#!/bin/bash
set -e

# 获取脚本所在绝对路径
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="${SCRIPT_DIR}"

echo "项目根目录: ${PROJECT_ROOT}"

# 设置构建目录
BUILD_DIR="${PROJECT_ROOT}/build"
echo "构建目录: ${BUILD_DIR}"

# 清理或创建构建目录
if [ -d "${BUILD_DIR}" ]; then
    echo "清理构建目录..."
    sudo rm -rf "${BUILD_DIR}"
fi

mkdir -p "${BUILD_DIR}"

# 进入构建目录
cd "${BUILD_DIR}"

# 执行CMake配置
echo "配置CMake..."
cmake "${PROJECT_ROOT}" \
    -DBUILD_LIB=OFF \
    -DBUILD_TOOLS=OFF \
    -DBUILD_TESTS=OFF

# 编译项目
echo "开始编译..."
make -j$(nproc)

# 查找可执行文件
EXECUTABLE=$(find "${BUILD_DIR}" -name ChineseToyCompiler -type f -executable | head -n 1)

if [ -z "${EXECUTABLE}" ]; then
    echo "错误: 未找到可执行文件ChineseToyCompiler"
    exit 1
fi

echo "找到可执行文件: ${EXECUTABLE}"

# 查找测试文件
TEST_FILE="${PROJECT_ROOT}/tests/examples/test.txt"

if [ ! -f "${TEST_FILE}" ]; then
    echo "警告：未找到测试文件 ${TEST_FILE}"
    echo "编译成功，但未执行测试"
    exit 0
fi

# 执行测试
echo "运行测试..."
"${EXECUTABLE}" "${TEST_FILE}"
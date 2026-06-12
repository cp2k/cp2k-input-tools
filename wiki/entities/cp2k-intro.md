# CP2K 介绍 / CP2K Introduction

## 概述 / Overview

CP2K 是一个用于量子化学和固体态物理的软件包，采用密度泛函理论 (DFT)、经典力场和混合方法进行原子模拟。它特别适用于：

- 分子动力学模拟
- 几何优化
- 能量计算
- 光谱计算
- 蒙特卡洛模拟

CP2K 由 CP2K 基金会开发维护，采用 GPLv3 开源许可。

CP2K is a software package for quantum chemistry and solid-state physics, performing atomistic simulations using Density Functional Theory (DFT), classical force fields, and hybrid methods. It is particularly suitable for:

- Molecular dynamics simulations
- Geometry optimization
- Energy calculations
- Spectral calculations
- Monte Carlo simulations

CP2K is developed and maintained by the CP2K Foundation under the GPLv3 open-source license.

## 关键属性 / Key Properties

### 计算方法 / Computational Methods

- **DFT 方法**：支持多种交换关联泛函，包括 LDA、GGA (PBE)、meta-GGA 和混合泛函
- **GW 方法**：用于准粒子能带结构计算
- **MP2 方法**：二阶微扰理论
- **经典力场**：支持多种力场格式，用于大规模分子动力学
- **混合方法**：QM/MM 耦合计算

### 并行性能 / Parallel Performance

- 支持 MPI 和 OpenMP 混合并行
- 优化的 FFT 库支持 (FFTW3, FFTSG)
- GPU 加速 (CUDA, HIP)
- 高效的 BLAS/LAPACK 集成

### 文件格式 / File Formats

- **输入文件**：`.inp` - CP2K 输入文件格式
- **基组文件**：BASIS_SET - 高斯基组定义
- **赝势文件**：POTENTIAL - 赝势定义
- **输出文件**：`.out` - 主输出文件
- **重启文件**：`.restart` - 用于继续计算
- **轨迹文件**：`.xyz`, `.dcd` - 轨迹数据

### 核心模块 / Core Modules

- **QuickStep**：DFT 计算引擎
- **FIST**：经典力场分子动力学
- **QS**：快速多极子方法
- **NEGF**：非平衡格林函数方法

### 程序类型 / Program Types

通过 `PROGRAM_NAME` 关键字指定：
- `CP2K` - 主计算程序
- `FARMING` - 批量任务运行
- `OPTIMIZE_INPUT` - 输入参数优化
- `OPTIMIZE_BASIS` - 基组优化

## 相关来源 / Related Sources

- **Parser 实现**：`cp2k_input_tools/parser.py` - CP2K 输入文件解析器
- **输入示例**：`tests/inputs/He_PBE.inp` - 氢原子 PBE 计算示例
- **XML 规范**：`cp2k_input_tools/cp2k_input.xml` - 完整输入语法规范
- **LSP 工具**：`cp2k-language-server` - 语言服务器支持

## 参考资料 / References

1. CP2K 官方手册：https://manual.cp2k.org/
2. CP2K 基金会：https://www.cp2k.org/
3. Hutter, J. et al. "cp2k: atomistic simulations of condensed matter systems" (2014)
4. VandeVondele, J. et al. "Quickstep: Fast and accurate density functional calculations" (2005)

## LSP 支持 / LSP Support

`cp2k-lsp-enhanced` 提供完整的语言服务器支持，包括：
- 输入文件语法高亮和验证
- 关键字和部分自动补全
- 实时错误检测
- 格式化和代码导航

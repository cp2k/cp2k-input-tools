# CP2K LSP Wiki 导航 / Navigation

## 概述 / Overview

这是 cp2k-lsp-enhanced 项目的知识库，包含 CP2K 量子化学和固体态物理模拟的完整参考。

## 目录结构 / Structure

```
wiki/
├── entities/       # 实体页面 (CP2K 特定概念)
├── concepts/       # 概念页面 (跨领域思想)
├── constraints/    # 约束页面 (高级约束和限制)
└── synthesis/      # 综合页面 (API 参考、工作流)
```

## 核心主题 / Core Topics

### 基础 / Basics

- [CP2K 简介](wiki/entities/cp2k-intro.md) - CP2K 概述和特点
- [输入文件格式](wiki/entities/input-file-format.md) - .inp 文件格式详解
- [基组文件](wiki/entities/basis-set.md) - BASIS_SET 文件格式
- [赝势文件](wiki/entities/pseudopotential.md) - POTENTIAL 文件格式

### DFT 与计算方法 / DFT & Computational Methods

- [密度泛函理论](wiki/concepts/density-functional-theory.md) - DFT 基础
- [交换关联泛函](wiki/concepts/xc-functionals.md) - LDA, GGA, meta-GGA, 混合泛函
- [QuickStep 模块](wiki/entities/quickstep.md) - DFT 计算引擎
- [GW 方法](wiki/concepts/gw-method.md) - 准粒子能带结构
- [MP2 方法](wiki/concepts/mp2-method.md) - 二阶微扰理论

### 分子动力学 / Molecular Dynamics

- [FIST 模块](wiki/entities/fist.md) - 经典力场 MD
- [QM/MM 方法](wiki/concepts/qmmm.md) - 量子/分子力学混合
- [周期性边界条件](wiki/concepts/periodic-boundary-conditions.md) - PBC 设置

### 输入结构 / Input Structure

- [GLOBAL 部分](wiki/entities/global-section.md) - 全局设置
- [FORCE_EVAL 部分](wiki/entities/force-eval-section.md) - 力计算设置
- [SUBSYS 部分](wiki/entities/subsys-section.md) - 子系统定义
- [DFT 部分](wiki/entities/dft-section.md) - DFT 参数设置

### 高级主题 / Advanced Topics

- [几何优化](wiki/concepts/geometry-optimization.md) - 结构优化
- [蒙特卡洛模拟](wiki/concepts/monte-carlo.md) - MC 方法
- [NEGF 方法](wiki/entities/negf.md) - 非平衡格林函数

### 文件格式与数据 / File Formats & Data

- [输出文件](wiki/entities/output-files.md) - .out 文件格式
- [重启文件](wiki/entities/restart-files.md) - .restart 文件
- [轨迹文件](wiki/entities/trajectory-files.md) - .xyz, .dcd 格式

### API 与工具 / API & Tools

- [解析器 API](wiki/synthesis/parser-api.md) - CP2K 输入解析器接口
- [生成器 API](wiki/synthesis/generator-api.md) - 输入文件生成接口
- [LSP 功能](wiki/synthesis/lsp-features.md) - 语言服务器功能
- [命令行工具](wiki/synthesis/cli-tools.md) - cp2klint, fromcp2k, tocp2k 等
- [典型工作流](wiki/synthesis/typical-workflow.md) - 完整模拟流程

## 快速链接 / Quick Links

- [源代码](raw/assets/) - 原始源文件
- [LSP 实现](raw/assets/cp2k_input_tools/) - 语言服务器代码
- [解析器实现](raw/assets/cp2k_input_tools/parser.py) - 输入解析器代码
- [测试文件](raw/assets/inputs/) - 测试用例和示例输入
- [XML 规范](raw/assets/cp2k_input_tools/cp2k_input.xml) - 完整输入语法规范

## 相关项目 / Related Projects

- [OpenQC-VSCode](https://github.com/newtontech/OpenQC-VSCode) - VS Code 扩展集成
- [AiiDA-CP2K](https://github.com/aiidateam/aiida-cp2k) - AiiDA 工作流集成

## 贡献 / Contributing

欢迎贡献！请参考项目根目录的 `CONTRIBUTING.md`。

## 许可证 / License

本项目基于 Apache 2.0 许可证。CP2K 核心程序基于 GPLv3 许可证。

---

最后更新: 2026-06-12

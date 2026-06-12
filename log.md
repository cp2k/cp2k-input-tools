# LLM Wiki 变更日志 / Changelog

## 2026-06-12

### 初始创建 / Initial Creation

创建 cp2k-lsp-enhanced 项目的 LLM Wiki 知识库，包含：

#### 实体页面 (Entity Pages)
- [x] cp2k-intro.md - CP2K 简介
- [ ] input-file-format.md - 输入文件格式
- [ ] basis-set.md - 基组文件格式
- [ ] pseudopotential.md - 赝势文件格式
- [ ] quickstep.md - QuickStep DFT 模块
- [ ] fist.md - FIST 经典力场模块
- [ ] global-section.md - GLOBAL 输入部分
- [ ] force-eval-section.md - FORCE_EVAL 输入部分
- [ ] subsys-section.md - SUBSYS 输入部分
- [ ] dft-section.md - DFT 输入部分
- [ ] negf.md - NEGF 方法
- [ ] output-files.md - 输出文件格式
- [ ] restart-files.md - 重启文件格式
- [ ] trajectory-files.md - 轨迹文件格式

#### 概念页面 (Concept Pages)
- [ ] density-functional-theory.md - 密度泛函理论
- [ ] xc-functionals.md - 交换关联泛函
- [ ] gw-method.md - GW 方法
- [ ] mp2-method.md - MP2 方法
- [ ] qmmm.md - QM/MM 方法
- [ ] periodic-boundary-conditions.md - 周期性边界条件
- [ ] geometry-optimization.md - 几何优化
- [ ] monte-carlo.md - 蒙特卡洛模拟

#### 约束页面 (Constraint Pages)
- [ ] parser-constraints.md - 解析器约束和限制
- [ ] validation-constraints.md - 验证约束

#### 综合页面 (Synthesis Pages)
- [ ] parser-api.md - 输入解析器 API
- [ ] generator-api.md - 输入生成器 API
- [ ] lsp-features.md - LSP 功能详解
- [ ] cli-tools.md - 命令行工具参考
- [ ] typical-workflow.md - 典型模拟工作流

#### 基础文件
- [x] index.md - 知识库导航
- [x] log.md - 变更日志

#### 资源文件
- [x] raw/assets/ - 源代码和文档副本
- [x] raw/assets/inputs/ - 测试用例和示例输入
- [x] raw/assets/cp2k_input_tools/ - 核心工具实现
- [x] raw/assets/cp2k_input_tools/cp2k_input.xml - XML 语法规范

### 统计 / Statistics

- **总文件数**: 30+
- **实体页面**: 1/14 (已创建/计划)
- **概念页面**: 0/8
- **约束页面**: 0/2
- **综合页面**: 0/5
- **语言**: 双语 (中文/English)

### 覆盖范围 / Coverage

- CP2K 核心概念 (输入文件、基组、赝势)
- DFT 与计算方法 (DFT、泛函、GW、MP2)
- 分子动力学 (FIST、QM/MM)
- 输入结构 (GLOBAL、FORCE_EVAL、SUBSYS、DFT)
- 高级主题 (几何优化、蒙特卡洛、NEGF)
- 文件格式 (输出、重启、轨迹)
- LSP 功能与 API

### 待完成项 / TODO

- [ ] 创建所有计划中的实体页面
- [ ] 创建所有计划中的概念页面
- [ ] 创建所有计划中的约束页面
- [ ] 创建所有计划中的综合页面
- [ ] 补充更多测试用例和示例输入
- [ ] 添加更多代码注释和文档字符串

---

此日志遵循 Karpathy LLM Wiki 模式。

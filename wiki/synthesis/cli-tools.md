# 命令行工具文档 / Command-line Tools Documentation

## 概述 / Overview

cp2k-input-tools 提供了多个命令行工具，用于 CP2K 输入文件的验证、转换、生成和查询。

cp2k-input-tools provides multiple command-line tools for CP2K input file validation, conversion, generation, and querying.

## 工具列表 / Tool List

| 工具 / Tool | 功能 / Function | 输入 / Input | 输出 / Output |
|-------------|----------------|-------------|---------------|
| `cp2klint` | 输入文件检查 / Lint input files | `.inp` | 诊断消息 |
| `fromcp2k` | CP2K → JSON/YAML/AiiDA | `.inp` | `.json` / `.yaml` / Python 脚本 |
| `tocp2k` | JSON/YAML → CP2K | `.json` / `.yaml` | `.inp` |
| `cp2kgen` | 参数化输入生成 / Parametric generation | `.inp` + 表达式 | 多个 `.inp` |
| `cp2kget` | 值查询 / Value query | `.inp` | 键值 |
| `cp2k-language-server` | LSP 服务器 / LSP server | - | stdio |

## 工具详解 / Tool Details

### 1. cp2klint - 输入文件检查器 / Input File Linter

检查 CP2K 输入文件的语法和语义错误。

Checks CP2K input files for syntax and semantic errors.

#### 基本用法 / Basic Usage

```bash
cp2klint <file.inp>
```

#### 示例 / Example

```bash
$ cp2klint tests/inputs/unterminated_var.inp
Syntax error: unterminated variable, in tests/inputs/unterminated_var.inp:
line   36: @IF ${HP
               ~~~~^
```

#### 检查项目 / Check Items

- 语法错误（无效的截面/关键字）
- 未关闭的截面
- 变量定义错误
- 条件块错误
- 包含文件错误

### 2. fromcp2k - CP2K 转换器 / CP2K Converter

将 CP2K 输入文件转换为 JSON、YAML 或 AiiDA 脚本。

Converts CP2K input files to JSON, YAML, or AiiDA scripts.

#### 基本用法 / Basic Usage

```bash
fromcp2k [OPTIONS] <file>

Options:
  -f, --format [json|yaml|aiida-cp2k-calc]  输出格式
  -c, --canonical                           使用规范格式
  -b, --base-dir DIRECTORY                 @include 搜索路径
  -t, --trafo [auto|lower|upper]          键名转换
  -E, --set key=value                     预设变量值
  --xml FILE                              使用替代 XML 规范
```

#### 格式选项 / Format Options

##### JSON 输出 / JSON Output

```bash
fromcp2k --format json calc.inp > calc.json
```

```json
{
  "global": {
    "run_type": "energy",
    "print_level": "medium"
  },
  "force_eval": {
    "method": "quickstep",
    "dft": {
      "cutoff": 1000.0
    }
  }
}
```

##### YAML 输出 / YAML Output

```bash
fromcp2k --format yaml calc.inp > calc.yaml
```

```yaml
global:
  run_type: energy
  print_level: medium
force_eval:
  method: quickstep
  dft:
    cutoff: 1000.0
```

##### AiiDA 脚本输出 / AiiDA Script Output

```bash
fromcp2k --format aiida-cp2k-calc calc.inp > aiida_calc.py
```

生成的 Python 脚本包含 AiiDA 计算设置：

Generates a Python script with AiiDA calculation setup:

```python
from aiida.engine import run
from aiida.orm import load_code, Dict

parameters = Dict(
    dict={
        "FORCE_EVAL": {
            "DFT": {
                "MGRID": {
                    "CUTOFF": 1000.0,
                },
            },
        },
    }
)

builder = cp2k_code.get_builder()
builder.parameters = parameters
run(builder)
```

#### 规范格式 vs 简化格式 / Canonical vs Simplified

```bash
# 简化格式（默认）
fromcp2k calc.inp

# 规范格式（保留所有列表）
fromcp2k --canonical calc.inp
```

#### 预设变量 / Preset Variables

```bash
fromcp2k --set CUTOFF=800 --set BASIS=DZVP-MOLOPT-SR template.inp
```

### 3. tocp2k - JSON/YAML 转换器 / JSON/YAML Converter

将 JSON 或 YAML 转换回 CP2K 输入文件。

Converts JSON or YAML back to CP2K input files.

#### 基本用法 / Basic Usage

```bash
tocp2k [-y] <file>

Options:
  -y, --yaml    输入为 YAML 格式（默认自动检测）
```

#### 示例 / Example

```bash
# JSON 转 CP2K
tocp2k calc.json > calc.inp

# YAML 转 CP2K
tocp2k calc.yaml > calc.inp
```

### 4. cp2kgen - 参数化生成器 / Parametric Generator

基于表达式生成多个 CP2K 输入文件。

Generates multiple CP2K input files based on expressions.

#### 基本用法 / Basic Usage

```bash
cp2kgen <template.inp> "<expression>"
```

#### 表达式语法 / Expression Syntax

使用路径语法指定要修改的参数：

Use path syntax to specify parameters to modify:

```bash
cp2kgen template.inp "force_eval/dft/mgrid/cutoff=[800,900,1000]"
```

生成文件：
- `template-cutoff_800.inp`
- `template-cutoff_900.inp`
- `template-cutoff_1000.inp`

#### 多表达式组合 / Multiple Expression Combination

多个表达式将组合为笛卡尔积：

Multiple expressions combine as Cartesian product:

```bash
cp2kgen template.inp \
  "force_eval/dft/mgrid/cutoff=[800,1000]" \
  "force_eval/dft/scf/eps_scf=[1.0E-6,1.0E-8]"
```

生成 4 个文件（2×2）：
- `template-cutoff_800-eps_scf_1.0E-06.inp`
- `template-cutoff_800-eps_scf_1.0E-08.inp`
- `template-cutoff_1000-eps_scf_1.0E-06.inp`
- `template-cutoff_1000-eps_scf_1.0E-08.inp`

### 5. cp2kget - 值查询器 / Value Querier

从 CP2K 输入文件中查询特定值。

Queries specific values from CP2K input files.

#### 基本用法 / Basic Usage

```bash
cp2kget <file.inp> "<path>"
```

#### 路径语法 / Path Syntax

使用 `/` 分隔的路径表示截面和关键字：

Use `/`-separated paths for sections and keywords:

```bash
# 查询 CUTOFF
cp2kget calc.inp "force_eval/dft/mgrid/cutoff"
# 输出: 1000.0

# 查询数组元素（0-based）
cp2kget restart.inp "force_eval/subsys/cell/a/0"
# 输出: 5.64123539364476
```

### 6. cp2k-language-server - LSP 服务器 / LSP Server

提供 Language Server Protocol 支持。

Provides Language Server Protocol support.

#### 启动 / Starting

```bash
cp2k-language-server
```

#### 编辑器集成 / Editor Integration

服务器通过 stdio 通信，由编辑器的 LSP 客户端启动。

Server communicates via stdio, launched by editor's LSP client.

## 典型工作流 / Typical Workflow

### 工作流 1: 验证并转换 / Workflow 1: Validate and Convert

```bash
# 1. 检查输入文件
cp2klint calc.inp

# 2. 转换为 JSON 进行处理
fromcp2k calc.inp > calc.json

# 3. 使用 jq 修改参数
jq '.force_eval.dft.mgrid.cutoff = 1200' calc.json > calc_mod.json

# 4. 转回 CP2K 格式
tocp2k calc_mod.json > calc_new.inp

# 5. 再次验证
cp2klint calc_new.inp
```

### 工作流 2: 参数扫描 / Workflow 2: Parameter Scan

```bash
# 为截断能收敛性研究生成多个输入文件
cp2kgen template.inp "force_eval/dft/mgrid/cutoff=[600,700,800,900,1000]"

# 提交所有计算
for f in template-cutoff_*.inp; do
    cp2k.popt -i $f -o ${f%.inp}.out
done
```

### 工作流 3: AiiDA 集成 / Workflow 3: AiiDA Integration

```bash
# 生成 AiiDA 计算脚本
fromcp2k --format aiida-cp2k-calc calc.inp > aiida_run.py

# 编辑脚本（设置代码、结构等）
vim aiida_run.py

# 通过 AiiDA 运行
verdi run aiida_run.py
```

## 安装 / Installation

```bash
# 基础安装
pip install cp2k-input-tools

# YAML 支持
pip install cp2k-input-tools[yaml]

# LSP 支持
pip install cp2k-input-tools[lsp]

# 全部功能
pip install cp2k-input-tools[yaml,lsp]
```

## 参考资料 / References

- README: `/Users/yhm/Desktop/code/cp2k-lsp-enhanced/README.md`
- 命令实现: `/Users/yhm/Desktop/code/cp2k-lsp-enhanced/cp2k_input_tools/`

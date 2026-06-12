# 预处理器文档 / Preprocessor Documentation

## 概述 / Overview

CP2K 输入文件支持预处理器指令，用于变量替换、条件编译和文件包含。cp2k-input-tools 实现了完整的 CP2K 预处理器功能。

CP2K input files support preprocessor directives for variable substitution, conditional compilation, and file inclusion. cp2k-input-tools implements full CP2K preprocessor functionality.

## 预处理器指令 / Preprocessor Directives

### 变量定义 / Variable Definition

#### @SET

定义一个预处理器变量。

Define a preprocessor variable.

```cp2k
@SET var_name value
```

**示例 / Example:**
```cp2k
@SET CUTOFF 800
@SET BASIS DZVP-MOLOPT-SR-GTH

&FORCE_EVAL
  &DFT
    &MGRID
      CUTOFF @CUTOFF@
    &END MGRID
    &SUBSYS
      &KIND H
        BASIS_SET @BASIS@
      &END KIND
    &END SUBSYS
  &END DFT
&END FORCE_EVAL
```

#### 命令行预设 / Command-line Preset

使用 `fromcp2k` 的 `--set` 选项：

Using the `--set` option of `fromcp2k`:

```bash
fromcp2k --set CUTOFF=1000 --set BASIS=TZVP-MOLOPT-GTH template.inp > calc.inp
```

### 变量引用 / Variable Reference

#### ${var} 语法 / ${var} Syntax

推荐语法，支持默认值。

Recommended syntax, supports default values.

```cp2k
@SET CUTOFF 800

# 使用默认值
CUTOFF ${CUTOFF:-600}  # 如果 CUTOFF 未定义，使用 600
```

#### $var 语法 / $var Syntax

简单语法，遇到空格时结束。

Simple syntax, ends at whitespace.

```cp2k
@SET VAR hello
VALUE @VAR@  # 替换为 "hello"
```

### 条件块 / Conditional Blocks

#### @IF / @ENDIF

条件编译指令。

Conditional compilation directives.

```cp2k
@IF ${VARIABLE} == value
  &SECTION
    KEYWORD value
  &END SECTION
@ENDIF

@IF ${VARIABLE} /= other_value
  &SECTION
    KEYWORD other_value
  &END SECTION
@ENDIF
```

**条件运算符 / Conditional Operators:**
- `==` : 相等 / Equal
- `/=` : 不相等 / Not equal
- 空或 `0` : False
- 任何非空非零值 : True

**示例 / Example:**
```cp2k
@SET CALC_TYPE ENERGY

@IF ${CALC_TYPE} == ENERGY
  &GLOBAL
    RUN_TYPE ENERGY
  &END GLOBAL
@ENDIF

@IF ${CALC_TYPE} == GEO_OPT
  &GLOBAL
    RUN_TYPE GEO_OPT
  &END GLOBAL
  &MOTION
    &GEO_OPT
      OPTIMIZER BFGS
    &END GEO_OPT
  &END MOTION
@ENDIF
```

### 文件包含 / File Inclusion

#### @INCLUDE

包含外部文件。

Include external files.

```cp2k
@INCLUDE "fragment.inp"
```

**搜索路径 / Search Path:**
1. 当前目录 / Current directory
2. `--base-dir` 指定的目录 / Directory specified by `--base-dir`

#### @XCTYPE

包含 XC 截面定义文件（从 `xc_section/` 目录）。

Include XC section definition files (from `xc_section/` directory).

```cp2k
@XCTYPE PBE
```

等价于 / Equivalent to:
```cp2k
@INCLUDE "xc_section/PBE.sec"
```

## 预处理器工作流 / Preprocessor Workflow

```
原始输入文件    →    预处理器    →    解析后的文件    →    解析器
Raw Input          Preprocessor        Processed         Parser
                                       File
```

### 处理顺序 / Processing Order

1. **变量解析**: 替换所有 `@VAR@` 和 `${VAR}` 引用
2. **条件块**: 根据 @IF 条件跳过或保留代码块
3. **文件包含**: 插入 @INCLUDE/@XCTYPE 指定的文件内容
4. **空行和注释**: 移除空行和 `!` 开头的注释
5. **传递给解析器**: 将处理后的内容传递给 CP2KInputParser

## 高级用法 / Advanced Usage

### 1. 嵌套变量 / Nested Variables

```cp2k
@SET BASE_DIR /path/to
@SET DATA_DIR ${BASE_DIR}/data

@INCLUDE "${DATA_DIR}/coords.inp"
```

### 2. 计算模板 / Calculation Templates

创建可重用的计算模板：

Create reusable calculation templates:

```cp2k
# template.inp
@SET PROJECT_NAME ${PROJECT:-calculation}
@SET CUTOFF ${CUTOFF:-800}
@SET BASIS ${BASIS:-DZVP-MOLOPT-SR-GTH}

&FORCE_EVAL
  METHOD QuickStep
  &DFT
    BASIS_SET_FILE_NAME ./BASIS_SETS
    POTENTIAL_FILE_NAME ./POTENTIALS
    &MGRID
      CUTOFF @CUTOFF@
    &END MGRID
    &SUBSYS
      @INCLUDE "kinds.inp"
    &END SUBSYS
  &END DFT
&END FORCE_EVAL

&GLOBAL
  PROJECT_NAME @PROJECT_NAME@
  RUN_TYPE @RUN_TYPE:-ENERGY@
&END GLOBAL
```

使用模板：

Using the template:

```bash
# 水分子计算
fromcp2k \
  --set PROJECT=H2O \
  --set CUTOFF=1000 \
  --set BASIS=TZVP-MOLOPT-GTH \
  template.inp > h2o.inp

# 甲醇计算
fromcp2k \
  --set PROJECT=methanol \
  --set CUTOFF=1200 \
  template.inp > methanol.inp
```

### 3. 条件编译 / Conditional Compilation

```cp2k
@SET USE_CONSTRAINTS 1

&FORCE_EVAL
  &SUBSYS
    &COORD
      H 0.0 0.0 0.0
      O 0.0 0.0 1.0
    &END COORD

    @IF ${USE_CONSTRAINTS} == 1
      &CONSTRAINT
        &COLLECTIVE
          &RESTRAINT
            ...
          &END RESTRAINT
        &END COLLECTIVE
      &END CONSTRAINT
    @ENDIF
  &END SUBSYS
&END FORCE_EVAL
```

### 4. 模块化输入 / Modular Input

将大型输入文件拆分为模块：

Split large input files into modules:

```
project/
├── main.inp
├── sections/
│   ├── force_eval.inp
│   ├── dft.inp
│   └── subsys.inp
└── fragments/
    ├── h2o_coords.inp
    └── kinds.inp
```

**main.inp:**
```cp2k
@SET RUN_TYPE ENERGY

@INCLUDE "sections/force_eval.inp"
@INCLUDE "sections/global.inp"
```

**sections/force_eval.inp:**
```cp2k
&FORCE_EVAL
  METHOD QuickStep
  @INCLUDE "sections/dft.inp"
  @INCLUDE "sections/subsys.inp"
&END FORCE_EVAL
```

## 错误处理 / Error Handling

### 常见错误 / Common Errors

#### 1. 未终止的变量 / Unterminated Variable

```cp2k
# ❌ 错误
VALUE ${UNCLOSED_VAR

# 错误信息：
# Syntax error: unterminated variable
# line   10: VALUE ${UNCLOSED_VAR
#               ~~~~~~~~~~~~~~~~
```

#### 2. 未定义的变量 / Undefined Variable

```cp2k
# ❌ 错误（无默认值）
VALUE ${UNDEFINED_VAR}

# 错误信息：
# Syntax error: undefined variable 'UNDEFINED_VAR' (and no default given)

# ✅ 正确（提供默认值）
VALUE ${UNDEFINED_VAR:-default_value}
```

#### 3. 嵌套条件块 / Nested Conditional Blocks

CP2K 不支持嵌套的 @IF/@ENDIF：

CP2K does not support nested @IF/@ENDIF:

```cp2k
# ❌ 错误
@IF ${A} == 1
  @IF ${B} == 2
    ...
  @ENDIF
@ENDIF

# 错误信息：
# Syntax error: nested @IF are not allowed
```

#### 4. 未关闭的条件块 / Unclosed Conditional Block

```cp2k
# ❌ 错误
@IF ${VAR} == 1
  &SECTION
    KEYWORD value
  &END SECTION
# 缺少 @ENDIF

# 错误信息：
# Syntax error: conditional block not closed at end of file
```

## 预处理器 API / Preprocessor API

### Python 使用 / Python Usage

```python
from cp2k_input_tools.parser import CP2KInputParser

# 设置初始变量
initial_vars = {
    "CUTOFF": "800",
    "BASIS": "DZVP-MOLOPT-SR-GTH",
    "RUN_TYPE": "ENERGY"
}

# 解析（预处理器自动处理）
parser = CP2KInputParser(base_dir=".")
with open("template.inp") as f:
    tree = parser.parse(f, initial_variable_values=initial_vars)
```

### 预处理器类 / Preprocessor Class

```python
from cp2k_input_tools.preprocessor import CP2KPreprocessor

preprocessor = CP2KPreprocessor(
    fhandle=open("input.inp"),
    base_dir=".",
    initial_variable_values={"VAR": "value"}
)

# 预处理器是迭代器
for line in preprocessor:
    print(line)  # 处理后的行
```

## 参考资料 / References

- CP2K 手册 - 输入预处理: https://manual.cp2k.org/user_guide/input.html
- 预处理器实现: `/Users/yhm/Desktop/code/cp2k-lsp-enhanced/cp2k_input_tools/preprocessor.py`
- 解析器文档: `/Users/yhm/Desktop/code/cp2k-lsp-enhanced/wiki/synthesis/parser-api.md`

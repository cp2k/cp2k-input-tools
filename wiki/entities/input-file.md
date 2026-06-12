# 输入文件格式 / Input File Format

## 概述 / Overview

CP2K 使用基于文本的输入文件格式 (`.inp`)，采用层级结构和类似 Fortran 的语法。输入文件由多个嵌套的部分 (section) 和关键字 (keyword) 组成。

CP2K uses a text-based input file format (`.inp`) with hierarchical structure and Fortran-like syntax. Input files consist of nested sections and keywords.

## 关键属性 / Key Properties

### 文件结构 / File Structure

```cp2k
&SECTION_NAME              # 开始部分 (section start)
  KEYWORD value           # 关键字赋值
  &SUBSECTION             # 子部分
    SUB_KEY value        # 子部分关键字
  &END SUBSECTION         # 结束子部分
&END SECTION_NAME         # 结束部分 (section end)
```

### 部分定义 / Section Definition

- 以 `&` 开头，后跟部分名称
- 可以接受参数：`&KIND H`
- 支持重复：部分可重复出现
- 嵌套：部分可包含子部分
- 以 `&END` 结束

### 关键字定义 / Keyword Definition

- 格式：`KEYWORD value1 value2 ...`
- 支持单位：`COORD [angstrom] 1.0 2.0 3.0`
- 支持重复：关键字可多次出现
- 默认关键字：某些部分支持默认关键字 (用 `*` 表示)

### 预处理器 / Preprocessor

CP2K 输入文件支持预处理指令：

- `@SET var value` - 定义变量
- `${var}` - 变量替换
- `@INCLUDE file` - 包含其他文件
- `@IF/@ELSE/@ENDIF` - 条件编译
- `@XCTYPE` - 包含 XC 函数定义

示例：
```cp2k
@SET CUTOFF_VAL 300
&MGRID
  CUTOFF ${CUTOFF_VAL}
&END MGRID

@IF ${INCLUDE_MPI}
  BLACS_GRID SQUARE
@ENDIF
```

### 注释 / Comments

- 以 `!`, `#` 开头的行视为注释
- 注释可以出现在行首或行尾

## 相关来源 / Related Sources

### Parser 实现

`parser.py` 中的关键类：

- `CP2KInputParser` - 标准解析器
- `CP2KInputParserSimplified` - 简化输出解析器
- `CP2KInputParserAiiDA` - AiiDA 兼容解析器

### 解析规则

```python
_SECTION_MATCH = re.compile(r"&(?P<name>[\w\-_]+)\s*(?P<param>.*)")
_KEYWORD_MATCH = re.compile(r"(?P<name>[\w\-_]+)\s*(?P<value>.*)")
```

### XML 规范

`cp2k_input.xml` 定义完整的输入语法结构，包括：
- 部分定义 (SECTION)
- 关键字定义 (KEYWORD)
- 数据类型 (DATA_TYPE)
- 默认值 (DEFAULT_VALUE)
- 重复属性 (repeats)

## JSON/YAML 转换 / JSON/YAML Conversion

### Canonical 格式

- 重复部分映射到列表
- 部分参数映射到 `_` 键
- 默认关键字映射到 `*` 键
- 部分前缀 `+` 区分关键字

### Simplified 格式

- 单元素列表简化为单个值
- 仅在有歧义时使用 `+` 前缀
- 参数化部分可用字典表示

## 示例 / Example

```cp2k
&GLOBAL
  PROJECT_NAME H2
  PRINT_LEVEL MEDIUM
  RUN_TYPE ENERGY
&END GLOBAL

&FORCE_EVAL
  METHOD Quickstep
  &DFT
    BASIS_SET_FILE_NAME BASIS_SET
    POTENTIAL_FILE_NAME POTENTIAL
    LSD
    &MGRID
      CUTOFF 140
    &END MGRID
    &SCF
      EPS_SCF 1.0E-4
      MAX_SCF 30
      SCF_GUESS atomic
    &END SCF
    @XCTYPE PBE
  &END DFT
  &SUBSYS
    &CELL
      ABC 8.0 4.0 4.0
    &END CELL
    &COORD
    H  0.0  0.0  0.0
    H  1.0  0.0  0.0
    &END COORD
    &KIND H
      BASIS_SET DZV-GTH-PADE
      POTENTIAL GTH-PADE-q1
    &END KIND
  &END SUBSYS
&END FORCE_EVAL
```

## 参考资料 / References

1. CP2K 输入参考手册：https://manual.cp2k.org/
2. cp2k-input-tools 仓库：https://github.com/cp2k/cp2k-input-tools

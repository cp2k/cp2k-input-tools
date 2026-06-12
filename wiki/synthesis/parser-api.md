# CP2KInputParser API 文档 / CP2KInputParser API Documentation

## 概述 / Overview

`CP2KInputParser` 是 cp2k-input-tools 的核心解析器，用于将 CP2K 输入文件解析为嵌套字典结构。该解析器支持完整的 CP2K 输入语法，包括预处理指令、变量替换、条件块和文件包含。

`CP2KInputParser` is the core parser of cp2k-input-tools, converting CP2K input files into nested dictionary structures. It supports full CP2K input syntax including preprocessor directives, variable substitution, conditional blocks, and file inclusion.

## 类层次 / Class Hierarchy

```
CP2KInputParser
    ├── CP2KInputParserSimplified
    │   └── CP2KInputParserAiiDA
    └── CP2KInputGenerator
```

## API / API

### CP2KInputParser

基础解析器类，生成符合规范的嵌套字典结构。

Base parser class that produces canonical nested dictionary structure.

#### 构造函数 / Constructor

```python
CP2KInputParser(xmlspec=None, base_dir=".", key_trafo=str.lower)
```

**参数 / Parameters:**
- `xmlspec` (str, optional): CP2K 输入 XML 规范文件路径，默认使用内置版本
- `base_dir` (str): 用于解析 `@include` 指令的基础目录
- `key_trafo` (callable): 用于转换键名的函数（大小写处理）

#### 主要方法 / Main Methods

##### parse()

```python
parse(fhandle, initial_variable_values=None) -> dict
```

解析 CP2K 输入文件。

Parse a CP2K input file.

**参数 / Parameters:**
- `fhandle`: 打开的文件句柄
- `initial_variable_values` (dict, optional): 预处理器变量的初始值

**返回 / Returns:** 嵌套字典结构的解析树

**示例 / Example:**
```python
from cp2k_input_tools.parser import CP2KInputParser

parser = CP2KInputParser()
with open("calc.inp") as f:
    tree = parser.parse(f)
    # tree = {"global": {...}, "force_eval": [...]}
```

##### coords()

```python
coords(force_eval=0) -> Iterator[Tuple[str, Tuple[float, ...], Optional[str]]]
```

返回坐标信息的迭代器。

Returns an iterator for coordinate information.

**返回 / Returns:**
- `(name, position, molecule_name)` 元组的迭代器
- `position` 自动转换为埃（Å）单位

**示例 / Example:**
```python
for atom_name, (x, y, z), mol_name in parser.coords():
    print(f"{atom_name}: {x:.3f} {y:.3f} {z:.3f}")
```

##### nested_dict (属性)

返回解析树的嵌套字典表示。

Returns the nested dictionary representation of the parsed tree.

### CP2KInputParserSimplified

简化的解析器，生成更易读的输出格式。

Simplified parser that produces more readable output format.

#### 构造函数 / Constructor

```python
CP2KInputParserSimplified(
    multi_value_unpack=True,
    repeated_section_unpack=True,
    level_reduction_blacklist=None,
    default_keyword_symbol="*",
    *args,
    **kwargs
)
```

**参数 / Parameters:**
- `multi_value_unpack`: 是否解包多值关键字
- `repeated_section_unpack`: 是否解包重复截面
- `level_reduction_blacklist`: 不进行层级简化的截面列表
- `default_keyword_symbol`: 默认关键字的符号

#### 输出格式差异 / Output Format Differences

| 特性 / Feature | 规范格式 / Canonical | 简化格式 / Simplified |
|----------------|---------------------|---------------------|
| 重复截面 | 始终为列表 / Always list | 单一截面时为字典 / Dict when single |
| 截面前缀 | 必需 `+` / Required `+` | 仅冲突时需要 / Only when conflicts |
| 多值关键字 | 始终为列表 / Always list | 单一值时为标量 / Scalar when single |

### CP2KInputParserAiiDA

专门为 AiiDA-cp2k 插件定制的解析器。

Parser specifically customized for the AiiDA-cp2k plugin.

```python
parser = CP2KInputParserAiiDA()
# 配置 / Configuration:
# - key_trafo=str.upper (键名大写)
# - multi_value_unpack=False (不解包多值)
# - repeated_section_unpack=False (不解包重复截面)
# - default_keyword_symbol=" " (空格作为默认关键字符号)
```

### CP2KInputGenerator

将嵌套字典反向生成 CP2K 输入文件。

Converts nested dictionaries back to CP2K input files.

#### 构造函数 / Constructor

```python
CP2KInputGenerator(xmlspec=DEFAULT_CP2K_INPUT_XML, indent_shift=3)
```

**参数 / Parameters:**
- `xmlspec`: CP2K 输入 XML 规范文件路径
- `indent_shift`: 缩进空格数

#### 主要方法 / Main Methods

##### line_iter()

```python
line_iter(tree) -> Iterator[str]
```

生成 CP2K 输入文件的行。

Generates lines of a CP2K input file.

**参数 / Parameters:**
- `tree`: 嵌套字典（来自 parser 的输出）

**返回 / Returns:** 字符串迭代器，每行为 CP2K 输入文件的一行

**示例 / Example:**
```python
from cp2k_input_tools.generator import CP2KInputGenerator

generator = CP2KInputGenerator()
tree = {"global": {"run_type": "energy"}}

with open("output.inp", "w") as f:
    for line in generator.line_iter(tree):
        f.write(f"{line}\n")
```

## 使用示例 / Examples

### 示例 1: 基本解析 / Example 1: Basic Parsing

```python
from cp2k_input_tools.parser import CP2KInputParserSimplified

# 解析输入文件
parser = CP2KInputParserSimplified()
with open("water.inp") as f:
    tree = parser.parse(f)

# 访问解析结果
print(tree["global"]["run_type"])  # "ENERGY"
print(tree["force_eval"]["dft"]["mgrid"]["cutoff"])  # 1000.0
```

### 示例 2: 带变量替换的解析 / Example 2: Parsing with Variable Substitution

```python
# 设置预处理器变量
initial_vars = {
    "CUTOFF": "800",
    "BASIS": "DZVP-MOLOPT-SR-GTH"
}

parser = CP2KInputParserSimplified()
with open("template.inp") as f:
    tree = parser.parse(f, initial_variable_values=initial_vars)
```

### 示例 3: 往返转换 / Example 3: Round-trip Conversion

```python
from cp2k_input_tools.parser import CP2KInputParserSimplified
from cp2k_input_tools.generator import CP2KInputGenerator

# 读取并解析
parser = CP2KInputParserSimplified()
with open("input.inp") as f:
    tree = parser.parse(f)

# 修改参数
tree["force_eval"]["dft"]["mgrid"]["cutoff"] = 1200.0

# 重新生成
generator = CP2KInputGenerator()
with open("modified.inp", "w") as f:
    for line in generator.line_iter(tree):
        f.write(f"{line}\n")
```

### 示例 4: 坐标提取 / Example 4: Coordinate Extraction

```python
parser = CP2KInputParser()
with open("geom.inp") as f:
    parser.parse(f)

# 提取坐标（自动转换为埃）
coords = list(parser.coords())
print(f"原子数: {len(coords)}")
for name, (x, y, z), _ in coords:
    print(f"{name}: {x:.4f} {y:.4f} {z:.4f} Å")
```

## 规范格式 vs 简化格式 / Canonical vs Simplified Format

### 规范格式 / Canonical Format

严格一对一映射到 CP2K 输入语法：

Strict one-to-one mapping to CP2K input syntax:

```json
{
  "+global": {
    "run_type": ["energy"]
  },
  "+force_eval": [
    {
      "method": ["quickstep"],
      "+dft": {
        "cutoff": [1000.0]
      }
    }
  ]
}
```

### 简化格式 / Simplified Format

更人性化的输出，省略不必要的列表：

More human-readable output, omitting unnecessary lists:

```json
{
  "global": {
    "run_type": "energy"
  },
  "force_eval": {
    "method": "quickstep",
    "dft": {
      "cutoff": 1000.0
    }
  }
}
```

## 错误处理 / Error Handling

解析器会抛出以下异常：

The parser may throw the following exceptions:

- `InvalidSectionError`: 无效的截面名称
- `InvalidKeywordError`: 无效的关键字名称
- `InvalidParameterError`: 无效的参数值
- `SectionMismatchError`: 截面不匹配（未关闭）
- `NameRepetitionError`: 重复的名称（不允许重复的关键字）
- `PreprocessorError`: 预处理器错误（变量、条件块、包含）

## 参考资料 / References

- CP2K 输入规范: https://manual.cp2k.org/
- cp2k-input-tools 源代码: `/Users/yhm/Desktop/code/cp2k-lsp-enhanced/cp2k_input_tools/parser.py`

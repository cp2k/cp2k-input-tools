# LSP 功能文档 / LSP Features Documentation

## 概述 / Overview

cp2k-input-tools 提供了完整的 Language Server Protocol (LSP) 实现，为 CP2K 输入文件提供现代编辑器支持，包括代码补全、语法检查、悬停文档等功能。

cp2k-input-tools provides a complete Language Server Protocol (LSP) implementation offering modern editor support for CP2K input files, including code completion, syntax checking, hover documentation, and more.

## LSP 功能 / LSP Features

### 1. 代码补全 / Code Completion

基于 CP2K 架构和光标上下文的智能补全。

Intelligent completion based on CP2K schema and cursor context.

#### 补全类型 / Completion Types

| 类型 / Type | 触发条件 / Trigger | 示例 / Example |
|-------------|-------------------|---------------|
| 截面补全 / Section | 输入 `&` 后 | `&FO` → `&FORCE_EVAL` |
| 关键字补全 / Keyword | 截面内输入 | `CUTO` → `CUTOFF` |
| 枚举值补全 / Enum | 关键字值位置 | `PBE` → `PBE0`, `PBESOL` |
| 逻辑值补全 / Logical | 布尔关键字 | `F`, `.FALSE.`, `T`, `.TRUE.` |

#### 补全实现 / Completion Implementation

```python
# completion.py
def get_completions(text: str, position: Position, uri: str) -> Optional[CompletionList]:
    """获取光标位置的补全项"""
    ctx = resolve_cursor_context(text, position.line, position.character, uri)
    schema = get_schema_index()

    items = []

    # 截面补全（& 后）
    if ctx.is_section_start:
        items.extend(_complete_sections(schema, ctx))

    # 关键字补全（截面内）
    elif ctx.current_section and not ctx.is_section_end:
        if ctx.is_value_position:
            items.extend(_complete_values(schema, ctx))
        else:
            items.extend(_complete_keywords(schema, ctx))

    return CompletionList(is_incomplete=False, items=items) if items else None
```

### 2. 诊断与验证 / Diagnostics & Validation

LSP 提供两类验证：语法验证和语义验证。

LSP provides two types of validation: syntax and semantic.

#### 语法验证 / Syntax Validation

自动检测并报告以下错误：

Automatically detects and reports the following errors:

- 语法错误（无效的截面/关键字）
- 未关闭的截面
- 无效的参数值
- 预处理器错误（未终止的变量、未关闭的条件块）

**示例诊断 / Example Diagnostic:**
```python
Diagnostic(
    range=Range(start=Position(line=10, character=5), end=Position(line=10, character=10)),
    message="invalid section 'FORCE_EVALL'",
    severity=DiagnosticSeverity.Error,
    source="cp2k-lsp"
)
```

#### 语义验证 / Semantic Validation

物理/化学感知的验证，检测语义上不正确但语法有效的输入：

Physics/chemistry-aware validation detecting semantically incorrect but syntactically valid inputs:

- `RUN_TYPE` 与 `MOTION` 截面冲突
- `METHOD` 与必需/禁止的截面不兼容
- 电子数与多重态不一致
- SCF 求解器冲突
- 截断能过低警告
- 已移除/废弃的关键字检测

**验证规则示例 / Validation Rule Example:**
```python
# validator.py
def _validate_run_type_motion(self, run_type: str, motion_section: Dict):
    """验证 RUN_TYPE 与 MOTION 截面的一致性"""
    if run_type == "GEO_OPT":
        forbidden = {"MD", "CELL_OPT", "BAND"}
        present = set(motion_section.keys()) & forbidden
        if present:
            self.diagnostics.append(SemanticDiagnostic(
                message=f"`RUN_TYPE=GEO_OPT` 与 `&MOTION / &{present}` 截面矛盾",
                severity="error"
            ))
```

### 3. 悬停文档 / Hover Documentation

鼠标悬停在关键字或截面上时显示描述性文档。

Shows descriptive documentation when hovering over keywords or sections.

**实现位置 / Implementation Location:** `ls.py` 中的悬停处理（待实现）

### 4. 光标上下文解析 / Cursor Context Resolution

理解光标在 CP2K 输入文件中的位置。

Understands the cursor position within a CP2K input file.

#### CursorContext 数据结构

```python
@dataclass(frozen=True)
class CursorContext:
    uri: str                           # 文件 URI
    line: int                          # 0-based 行号
    character: int                     # 0-based 字符偏移
    section_path: Tuple[str, ...]      # 从根到当前截面的路径
    current_section: Optional[str]     # 最内层打开的截面
    current_keyword: Optional[str]     # 当前行上的关键字
    is_section_start: bool             # 光标是否在 & 后
    is_section_end: bool               # 光标是否在 &END 行
    is_keyword_position: bool          # 光标是否在关键字名上
    is_value_position: bool            # 光标是否在值位置
    prefix: str                        # 用于补全匹配的前缀
```

#### 上下文解析示例 / Context Resolution Example

```python
# 输入文件内容：
# &FORCE_EVAL
#    METHOD Quickstep
#    &DFT
#       CUTOFF 800|
#                   ^ cursor here

ctx = resolve_cursor_context(text, line=3, character=15, uri="file.inp")

# 结果：
print(ctx.section_path)        # ('FORCE_EVAL', 'DFT')
print(ctx.current_section)     # 'DFT'
print(ctx.current_keyword)     # 'CUTOFF'
print(ctx.is_value_position)   # True
print(ctx.prefix)              # '800'
```

## 使用 LSP / Using the LSP

### VS Code 配置 / VS Code Configuration

通过 OpenQC-VSCode 扩展使用：

Use via the OpenQC-VSCode extension:

```json
{
  "languages": [{
    "id": "cp2k",
    "extensions": [".inp"],
    "configuration": "cp2k-language-server"
  }]
}
```

### Vim 配置 / Vim Configuration

使用 ALE 插件：

Using the ALE plugin:

```vim
" ale_linters/cp2k/language_server.vim
call ale#Set('cp2k_lsp_executable', 'cp2k-language-server')

function! ale_linters#cp2k#language_server#GetProjectRoot(buffer) abort
    let l:git_path = ale#path#FindNearestDirectory(a:buffer, '.git')
    return !empty(l:git_path) ? fnamemodify(l:git_path, ':h:h') : ''
endfunction

call ale#linter#Define('cp2k', {
\   'name': 'language_server',
\   'lsp': 'stdio',
\   'executable': {b -> ale#Var(b, 'cp2k_lsp_executable')},
\   'project_root': function('ale_linters#cp2k#language_server#GetProjectRoot'),
\   'command': '%e',
\})
```

### 启动语言服务器 / Starting the Language Server

```bash
# 安装
pip install cp2k-input-tools[lsp]

# 启动（通常由编辑器自动启动）
cp2k-language-server
```

## LSP 服务器架构 / LSP Server Architecture

```python
# ls.py
from pygls.server import LanguageServer

cp2k_server = LanguageServer("cp2k-lsp", "v0.1")

@cp2k_server.feature(TEXT_DOCUMENT_DID_CHANGE)
def did_change(ls, params: DidChangeTextDocumentParams):
    """文档变更时验证"""
    _validate(ls, params)

@cp2k_server.feature(TEXT_DOCUMENT_COMPLETION)
def did_completion(ls, params: CompletionParams):
    """提供补全"""
    return completion(ls, params)

@cp2k_server.feature(TEXT_DOCUMENT_DID_OPEN)
async def did_open(ls, params: DidOpenTextDocumentParams):
    """文档打开时验证"""
    _validate(ls, params)
```

## 诊断消息示例 / Diagnostic Message Examples

### 语法错误 / Syntax Error

```
❌ Syntax error: invalid section 'FORCE_EVALL'
   at line 5: &FORCE_EVALL
               ~~~~~~~~~~~~~
```

### 语义错误 / Semantic Error

```
❌ `RUN_TYPE=GEO_OPT` (几何优化) 与 `&MOTION / &MD` 截面矛盾
   - 当前设置：RUN_TYPE=GEO_OPT
   - 检测到：&MD 截面
建议：
   - 若需分子动力学，请将 RUN_TYPE 改为 `MD`
   - 若需几何优化，请删除 &MOTION / &MD 截面
```

### 警告 / Warning

```
⚠️ 截断能过低可能导致结果不准确
   - 当前 CUTOFF：200 Ry
   - 建议值：≥ 300 Ry (生产计算)
   - 高精度：≥ 600 Ry
注意：截断能过低会导致基组不完整，能量和力计算误差增大
```

## 参考资料 / References

- LSP 规范: https://microsoft.github.io/language-server-protocol/
- pygls 文档: https://github.com/openlawlibrary/pygls
- cp2k-input-tools LSP 实现: `/Users/yhm/Desktop/code/cp2k-lsp-enhanced/cp2k_input_tools/ls.py`
- 补全模块: `/Users/yhm/Desktop/code/cp2k-lsp-enhanced/cp2k_input_tools/completion.py`
- 验证模块: `/Users/yhm/Desktop/code/cp2k-lsp-enhanced/cp2k_input_tools/validator.py`

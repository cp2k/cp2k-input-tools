# 典型 CP2K 工作流 / Typical CP2K Workflow

## 概述 / Overview

本文档描述了使用 cp2k-input-tools 进行 CP2K 计算的典型工作流程，包括输入文件准备、验证、参数优化和结果分析。

This document describes typical workflows for CP2K calculations using cp2k-input-tools, including input file preparation, validation, parameter optimization, and result analysis.

## 工作流阶段 / Workflow Stages

```
┌─────────────────┐    ┌─────────────────┐    ┌─────────────────┐
│  准备阶段        │ -> │  验证阶段        │ -> │  运行阶段        │
│  Preparation    │    │  Validation     │    │  Execution      │
└─────────────────┘    └─────────────────┘    └─────────────────┘
        │                       │                       │
        v                       v                       v
  创建/编辑输入文件            语法/语义检查            提交计算
  设置预处理器变量            LSP 实时反馈            监控进度
  参数化模板                  批量验证                收集结果
```

## 阶段 1: 准备 / Stage 1: Preparation

### 1.1 创建基本输入文件 / Creating Basic Input File

```cp2k
&FORCE_EVAL
  METHOD QuickStep
  &DFT
    BASIS_SET_FILE_NAME ./BASIS_SETS
    POTENTIAL_FILE_NAME ./POTENTIALS
    &MGRID
      CUTOFF 800
      REL_CUTOFF 80
    &END MGRID
    &SCF
      MAX_SCF 50
      EPS_SCF 1.0E-5
    &END SCF
    &XC
      &XC_FUNCTIONAL PBE
      &END XC_FUNCTIONAL
    &END XC
  &END DFT
  &SUBSYS
    &KIND H
      BASIS_SET DZVP-MOLOPT-SR-GTH
      POTENTIAL GTH-PBE
    &END KIND
    &COORD
      H 0.0 0.0 0.0
    &END COORD
  &END SUBSYS
&END FORCE_EVAL

&GLOBAL
  PROJECT_NAME H_atom
  RUN_TYPE ENERGY
&END GLOBAL
```

### 1.2 使用模板和变量 / Using Templates and Variables

创建带变量的模板文件：

Create template file with variables:

```cp2k
# template.inp
@SET CUTOFF ${CUTOFF:-800}
@SET BASIS ${BASIS:-DZVP-MOLOPT-SR-GTH}

&FORCE_EVAL
  METHOD QuickStep
  &DFT
    &MGRID
      CUTOFF @CUTOFF@  # 使用预处理器变量
    &END MGRID
    &SUBSYS
      &KIND H
        BASIS_SET @BASIS@
      &END KIND
    &END SUBSYS
  &END DFT
&END FORCE_EVAL
```

从命令行设置变量：

Set variables from command line:

```bash
fromcp2k --set CUTOFF=1000 --set BASIS=TZVP-MOLOPT-GTH template.inp > calc.inp
```

### 1.3 参数化生成 / Parametric Generation

为参数研究生成多个输入文件：

Generate multiple input files for parameter studies:

```bash
# 截断能扫描
cp2kgen template.inp "force_eval/dft/mgrid/cutoff=[600,800,1000,1200]"

# 多参数扫描（笛卡尔积）
cp2kgen template.inp \
  "force_eval/dft/mgrid/cutoff=[800,1000]" \
  "force_eval/dft/scf/eps_scf=[1.0E-6,1.0E-8]"
```

## 阶段 2: 验证 / Stage 2: Validation

### 2.1 使用 cp2klint 验证 / Validation with cp2klint

```bash
# 基本验证
cp2klint calc.inp

# 详细输出
cp2klint -v calc.inp

# 批量验证
for f in *.inp; do
    echo "Checking $f..."
    cp2klint "$f" || echo "FAILED: $f"
done
```

### 2.2 使用 LSP 实时验证 / Real-time Validation with LSP

在支持 LSP 的编辑器中：

In LSP-supporting editors:

1. **VS Code**: 安装 OpenQC-VSCode 扩展
2. **Vim**: 配置 ALE + cp2k-language-server
3. **Emacs**: 使用 eglot 或 lsp-mode

实时诊断示例：

Real-time diagnostics example:

```
┌─────────────────────────────────────────────────────┐
│ &FORCE_EVAL                                         │
│   METHOD Quickstep                                  │
│   &DFT                                              │
│     &MGRID                                          │
│       CUTOFF 100  ▼ Warning: 截断能过低            │
│       ~~~~~~                                        │
│     &END MGRID                                      │
│   &END DFT                                          │
│ &END FORCE_EVAL                                     │
└─────────────────────────────────────────────────────┘
```

### 2.3 语义验证检查项 / Semantic Validation Checks

#### 检查 1: RUN_TYPE 与 MOTION 一致性

```cp2k
# ❌ 错误示例
&GLOBAL
  RUN_TYPE GEO_OPT
&END GLOBAL
&MOTION
  &MD  # 与 GEO_OPT 矛盾
    ...
  &END MD
&END MOTION

# ✅ 正确示例
&GLOBAL
  RUN_TYPE GEO_OPT
&END GLOBAL
&MOTION
  &GEO_OPT
    ...
  &END GEO_OPT
&END MOTION
```

#### 检查 2: METHOD 与必需截面

```cp2k
# ❌ 错误示例
&FORCE_EVAL
  METHOD QuickStep
  # 缺少 &DFT 截面
&END FORCE_EVAL

# ✅ 正确示例
&FORCE_EVAL
  METHOD QuickStep
  &DFT
    ...
  &END DFT
&END FORCE_EVAL
```

#### 检查 3: 电子数与多重态

```cp2k
# ❌ 错误示例
&DFT
  CHARGE 0
  MULTIPLICITY 2  # 奇数电子需要奇数多重态
&END DFT
&SUBSYS
  &COORD
    H 0.0 0.0 0.0  # 1 个电子
  &END COORD
&END SUBSYS

# ✅ 正确示例（双原子氢分子）
&DFT
  CHARGE 0
  MULTIPLICITY 1
  UKS FALSE
&END DFT
&SUBSYS
  &COORD
    H -0.35 0.0 0.0
    H  0.35 0.0 0.0
  &END COORD
&END SUBSYS
```

## 阶段 3: 运行 / Stage 3: Execution

### 3.1 标准运行 / Standard Run

```bash
# 单节点运行
cp2k.popt -i calc.inp -o calc.out

# MPI 并行
mpirun -np 4 cp2k.psmp -i calc.inp -o calc.out

# 检查输出
grep -i "energy|" calc.out
```

### 3.2 批量运行 / Batch Execution

```bash
# 参数扫描批量运行
for f in template-cutoff_*.inp; do
    basename=${f%.inp}
    echo "Running $basename..."
    cp2k.popt -i "$f" -o "${basename}.out" &
done
wait

# 收集结果
for f in template-cutoff_*.out; do
    energy=$(grep "ENERGY|" "$f" | tail -1 | awk '{print $2}')
    basename=${f%.out}
    cutoff=$(echo $basename | sed 's/.*cutoff_//')
    echo "$cutoff $energy"
done > convergence.dat
```

### 3.3 收敛性研究 / Convergence Study

典型的截断能收敛性工作流：

Typical cutoff convergence workflow:

```bash
# 1. 生成输入文件
cp2kgen template.inp "force_eval/dft/mgrid/cutoff=[400,500,600,700,800,900,1000]"

# 2. 运行计算
for f in template-cutoff_*.inp; do
    nohup cp2k.popt -i "$f" -o "${f%.inp}.out" &
done

# 3. 分析结果
cat > analyze_convergence.py << 'EOF'
import re
import matplotlib.pyplot as plt

cutoffs = []
energies = []

for f in sorted("template-cutoff_*.out"):
    cutoff = int(re.search(r'cutoff_(\d+)', f).group(1))
    with open(f) as fh:
        for line in fh:
            if "ENERGY|" in line:
                energy = float(line.split()[1])
                cutoffs.append(cutoff)
                energies.append(energy)
                break

plt.plot(cutoffs, energies, 'o-')
plt.xlabel('CUTOFF (Ry)')
plt.ylabel('Energy (Hartree)')
plt.title('Cutoff Convergence')
plt.savefig('convergence.png')
EOF

python analyze_convergence.py
```

## 阶段 4: 结果提取 / Stage 4: Result Extraction

### 4.1 使用 cp2kget 提取值 / Using cp2kget to Extract Values

```bash
# 从 restart 文件提取优化后的晶胞参数
cp2kget restart.inp "force_eval/subsys/cell/a"
# 输出: 5.641 2.820 0.000

# 提取坐标
cp2kget restart.inp "force_eval/subsys/coord/*"
```

### 4.2 使用 Python API / Using Python API

```python
from cp2k_input_tools.parser import CP2KInputParserSimplified

# 解析结果
parser = CP2KInputParserSimplified()
with open("restart.wfn") as f:
    tree = parser.parse(f)

# 访问数据
cutoff = tree["force_eval"]["dft"]["mgrid"]["cutoff"]
print(f"Used cutoff: {cutoff} Ry")

# 提取坐标
for atom, coords, mol in parser.coords():
    print(f"{atom}: {coords}")
```

## 阶段 5: 迭代优化 / Stage 5: Iterative Optimization

### 5.1 基于结果修改输入 / Modifying Input Based on Results

```python
# 根据输出文件修改输入
from cp2k_input_tools.parser import CP2KInputParserSimplified
from cp2k_input_tools.generator import CP2KInputGenerator

# 解析原始输入
parser = CP2KInputParserSimplified()
with open("calc.inp") as f:
    tree = parser.parse(f)

# 从 restart 文件获取优化结构
parser2 = CP2KInputParserSimplified()
with open("geo_opt.restart") as f:
    restart_tree = parser2.parse(f)

# 更新结构
tree["force_eval"]["subsys"]["coord"] = restart_tree["force_eval"]["subsys"]["coord"]

# 生成新输入
generator = CP2KInputGenerator()
with open("single_point.inp", "w") as f:
    for line in generator.line_iter(tree):
        f.write(f"{line}\n")

# 验证新输入
import subprocess
subprocess.run(["cp2klint", "single_point.inp"])
```

## 完整示例: 几何优化 + 单点能 / Complete Example: Geometry Optimization + Single Point

```bash
# 1. 创建几何优化模板
cat > geo_opt_template.inp << 'EOF'
&FORCE_EVAL
  METHOD QuickStep
  &DFT
    BASIS_SET_FILE_NAME ./BASIS_SETS
    POTENTIAL_FILE_NAME ./POTENTIALS
    &MGRID
      CUTOFF @CUTOFF@
    &END MGRID
    &SCF
      MAX_SCF 100
      EPS_SCF 1.0E-6
    &END SCF
    &XC
      &XC_FUNCTIONAL PBE
      &END XC_FUNCTIONAL
    &END XC
  &END DFT
  &SUBSYS
    &KIND H
      BASIS_SET @BASIS@
      POTENTIAL GTH-PBE
    &END KIND
    &COORD
      H -0.35 0.0 0.0
      H  0.35 0.0 0.0
    &END COORD
  &END SUBSYS
&END FORCE_EVAL

&GLOBAL
  PROJECT_NAME H2_geo_opt
  RUN_TYPE GEO_OPT
&END GLOBAL
EOF

# 2. 设置参数
fromcp2k --set CUTOFF=800 --set BASIS=DZVP-MOLOPT-SR-GTH \
  geo_opt_template.inp > geo_opt.inp

# 3. 验证
cp2klint geo_opt.inp

# 4. 运行几何优化
cp2k.popt -i geo_opt.inp -o geo_opt.out

# 5. 提取优化结构
cp2kget H2_geo_opt-1.restart "force_eval/subsys/coord"

# 6. 创建单点能输入（使用优化结构）
# （手动或使用上面展示的 Python 脚本）

# 7. 运行高精度单点计算
cp2k.popt -i single_point.inp -o single_point.out
```

## 最佳实践 / Best Practices

1. **始终验证输入**: 使用 `cp2klint` 或 LSP 在运行前检查输入
2. **使用模板**: 将常用配置保存为模板，使用变量参数化
3. **渐进式收敛**: 从低精度开始测试，然后逐步提高精度
4. **版本控制**: 将输入文件和重要输出文件纳入版本控制
5. **文档化**: 在输入文件中使用注释记录计算目的和参数选择

## 参考资料 / References

- CP2K 手册: https://manual.cp2k.org/
- cp2k-input-tools README: `/Users/yhm/Desktop/code/cp2k-lsp-enhanced/README.md`

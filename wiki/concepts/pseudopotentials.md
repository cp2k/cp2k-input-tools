# 赝势 / Pseudopotentials

## 定义 / Definition

赝势（Pseudopotential, PP）是一种有效方法，用于减少量子化学计算中的电子数。通过将核心电子冻结并用有效势代替，只需显式处理价电子。这显著降低了计算成本，同时保持了化学精度。

A pseudopotential is an effective method to reduce the number of electrons in quantum chemistry calculations. By freezing core electrons and replacing them with an effective potential, only valence electrons need explicit treatment. This significantly reduces computational cost while maintaining chemical accuracy.

## 核心机制 / Core Mechanism

### 基本原理 / Basic Principle

全电子波函数：All-electron wave function:

```
ψ(r) = ψ_core(r) + ψ_valence(r)
```

赝波函数：Pseudowave function:

```
φ_pp(r) = ψ_valence(r)  (r > r_c)
φ_pp(r) = 平滑连接     (r < r_c)
```

其中 r_c 是截断半径。

### 赝势类型 / Pseudopotential Types

| 类型 / Type | 特点 / Characteristics | 应用 / Applications |
|-------------|------------------------|---------------------|
| 规范守恒 (NCPP) | 全电子波函数匹配，散射性质正确 | 适用于精确计算 |
| 超软 (USPP) | 赝波函数展开更小，平面波效率高 | 大型系统，DFT |
| 投影缀加波 (PAW) | 全电子信息可恢复，精度高 | 现代标准 |
| 模守恒 (MCP) | 保持规范守恒但数值更稳定 | 特殊系统 |

| Type | Characteristics | Applications |
|------|------------------|---------------------|
| Norm-Conserving (NCPP) | All-electron wave function matching, correct scattering | High-accuracy calculations |
| Ultrasoft (USPP) | Smaller pseudowave expansion, efficient plane waves | Large systems, DFT |
| Projector Augmented Wave (PAW) | All-electron information recoverable, high accuracy | Modern standard |
| Model-Conserving (MCP) | Norm-conserving but numerically stable | Special systems |

### GTH 赝势 / GTH Pseudopotentials

CP2K 使用 Goedecker-Teter-Hutter (GTH) 赝势：

CP2K uses Goedecker-Teter-Hutter (GTH) pseudopotentials:

```
V_pp(r) = V_local(r) + Σ_lm |p_lm⟩V_l⟨p_lm|
```

特点：Features:
- **局域势**: V_local(r) 包含长程库仑部分
- **非局域势**: 投影算符 ⟨p_lm| 描述角动量依赖
- **解析形式**: 分段多项式，计算高效

- **Local potential**: V_local(r) includes long-range Coulomb part
- **Non-local potential**: Projector ⟨p_lm| for angular momentum dependence
- **Analytical form**: Piecewise polynomial, computationally efficient

### PAW 方法 / PAW Method

PAW 将全电子和赝波函数关联：PAW relates all-electron and pseudowave functions:

```
|ψ⟩ = |φ⟩ + Σ_i (|ϕ_i⟩ - |φ̃_i⟩)⟨p̃_i|φ⟩
```

其中：Where:
- |ϕ_i⟩: 全电子部分波
- |φ̃_i⟩: 赝部分波
- ⟨p̃_i|: 投影函数

### CP2K 中的赝势设置 / Pseudopotential Settings in CP2K

```cp2k
&KIND H
  BASIS_SET DZV-GTH-PADE
  POTENTIAL GTH-PADE-q1    # q1 = 1 价电子
&END KIND

&KIND O
  BASIS_SET DZV-GTH-PADE
  POTENTIAL GTH-PADE-q6    # q6 = 6 价电子
&END KIND
```

| 赝势格式 / Format | 描述 / Description |
|-------------------|--------------------|
| GTH | Goedecker-Teter-Hutter，CP2K 标准 |
| GTH-PADE | Padé 近似优化的 GTH |
| SG (Short-range) | 短程赝势，周期系统 |
| PAW | 投影缀加波，高精度 |

| Format | Description |
|--------|-------------|
| GTH | Goedecker-Teter-Hutter, CP2K standard |
| GTH-PADE | Padé-approximation optimized GTH |
| SG (Short-range) | Short-range PP, periodic systems |
| PAW | Projector Augmented Wave, high accuracy |

## 应用场景 / Applications

- **重元素计算**: 减少过渡金属、镧系、锕系的电子数
- **大系统模拟**: 蛋白质、纳米材料、界面
- **周期性系统**: 晶体、表面、体相材料
- **分子动力学**: 减少每个原子的计算成本
- **高通量筛选**: 快速评估大量化合物

- **Heavy Elements**: Reduce electrons for transition metals, lanthanides, actinides
- **Large Systems**: Proteins, nanomaterials, interfaces
- **Periodic Systems**: Crystals, surfaces, bulk materials
- **Molecular Dynamics**: Reduce per-atom cost
- **High-Throughput Screening**: Rapid evaluation of many compounds

## 相关概念 / Related Concepts

- **基组**: 与赝势必须匹配
- **冻结芯近似**: 类似概念
- **有效核势 (ECP)**: 另一名称
- **全电子计算**: 不使用赝势的对比
- **半芯态**: 某些赝势包含的半核心电子

- **Basis Sets**: Must match pseudopotential
- **Frozen Core Approximation**: Similar concept
- **Effective Core Potential (ECP)**: Another name
- **All-Electron Calculation**: Contrast without PP
- **Semi-core States**: Some PPs include semi-core electrons

## 来源 / Sources

- CP2K Input Reference: KIND section, POTENTIAL keyword
- /Users/yhm/Desktop/code/cp2k-lsp-enhanced/tests/inputs/He_PBE.inp (line 33)
- Goedecker et al. (1996). "Separable dual-space Gaussian pseudopotentials"
- Blöchl (1994). "Projector augmented-wave method"

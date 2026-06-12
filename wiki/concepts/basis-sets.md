# 基组 / Basis Sets

## 定义 / Definition

基组是一组用于展开分子轨道的数学函数。在量子化学计算中，电子波函数表示为基函数的线性组合。基组的选择直接影响计算精度、效率和数值稳定性。

A basis set is a collection of mathematical functions used to expand molecular orbitals. In quantum chemistry calculations, electron wave functions are expressed as linear combinations of basis functions. The choice of basis set directly affects computational accuracy, efficiency, and numerical stability.

## 核心机制 / Core Mechanism

### 基组类型 / Basis Set Types

| 类型 / Type | 描述 / Description | 示例 / Examples |
|-------------|--------------------|-----------------|
| STO (Slater 型轨道) | 真实电子行为的解析形式 | 最小基组，少用 |
| GTO (高斯型轨道) | 计算效率高，乘积仍是高斯 | 所有现代计算 |
| 谐振基 | 数值基，周期系统高效 | CP2K 特有 |

| Type | Description | Examples |
|------|-------------|----------|
| STO (Slater-Type Orbitals) | Analytical form of real electron behavior | Minimal basis, rare |
| GTO (Gaussian-Type Orbitals) | High computational efficiency, product remains Gaussian | All modern calculations |
| Harmonic basis | Numerical basis, efficient for periodic systems | CP2K specific |

### 高斯基组构造 / Gaussian Basis Set Construction

原始高斯函数：Primitive Gaussian function:

```
g(r) = N · x^l · y^m · z^n · exp(-αr²)
```

收缩高斯函数：Contracted Gaussian function:

```
φ(r) = Σ_i d_i · g_i(r)
```

其中：Where:
- d_i: 收缩系数
- α: 指数参数
- (l,m,n): 角动量

### 基组层级 / Basis Set Hierarchy

| 层级 / Level | Pople 命名 / Pople Notation | Dunning 命名 / Dunning Notation | 描述 / Description |
|-------------|----------------------------|----------------------------------|--------------------|
| 最小基 / Minimal | STO-3G | - | 每个原子一个函数，定性结果 |
| 分裂价基 / Split-valence | 3-21G, 6-31G* | cc-pVDZ | 价层分裂，平衡精度和成本 |
| 三重分裂 / Triple-split | 6-311G** | cc-pVTZ | 高精度，大系统慎用 |
| 四重分裂 / Quadruple-split | - | cc-pVQZ | 近基准精度，计算昂贵 |

| Level | Pople Notation | Dunning Notation | Description |
|-------|-----------------|-------------------|-------------|
| Minimal | STO-3G | - | One function per atom, qualitative |
| Split-valence | 3-21G, 6-31G* | cc-pVDZ | Valence split, balance |
| Triple-split | 6-311G** | cc-pVTZ | High accuracy, careful for large systems |
| Quadruple-split | - | cc-pVQZ | Near-basis-set limit, expensive |

### CP2K 基组系统 / CP2K Basis Set System

CP2K 使用 GTH (Goedecker-Teter-Hutter) 基组和赝势：

CP2K uses GTH (Goedecker-Teter-Hutter) basis sets and pseudopotentials:

```cp2k
&KIND H
  BASIS_SET DZV-GTH-PADE     # 双-ζ 价基
  POTENTIAL GTH-PADE-q1       # 1 价电子赝势
&END KIND
```

| 基组代码 / Code | 含义 / Meaning |
|-----------------|----------------|
| DZV (Double-Zeta Valence) | 价层两个函数 |
| TZV (Triple-Zeta Valence) | 价层三个函数 |
| QZV (Quadruple-Zeta Valence) | 价层四个函数 |
| GTH | Goedecker-Teter-Hutter 格式 |
| PADE | Padé 近似优化 |

| Code | Meaning |
|------|---------|
| DZV (Double-Zeta Valence) | Two functions in valence |
| TZV (Triple-Zeta Valence) | Three functions in valence |
| QZV (Quadruple-Zeta Valence) | Four functions in valence |
| GTH | Goedecker-Teter-Hutter format |
| PADE | Padé approximation optimized |

### 弥散和极化函数 / Diffuse and Polarization Functions

- **极化函数 (+, **)**: 增加 d, f 轨道，描述电子变形
- **弥散函数 (+++)**: 指数小，描述阴离子和激发态
- **示例**: 6-31+G* = 极化 + 弥散

- **Polarization functions (+, **)**: Add d, f orbitals for electron deformation
- **Diffuse functions (+++)**: Small exponents for anions and excited states
- **Example**: 6-31+G* = Polarization + Diffuse

## 应用场景 / Applications

- **分子结构**: 平衡几何构型，键长，键角
- **能量计算**: 反应能，结合能，原子化能
- **光谱性质**: 振动频率，NMR 化学位移
- **周期系统**: 晶体结构，能带计算
- **基准测试**: 向完全基组极限外推

- **Molecular Structure**: Equilibrium geometry, bond lengths, angles
- **Energy Calculations**: Reaction energies, binding energies, atomization energies
- **Spectroscopic Properties**: Vibrational frequencies, NMR chemical shifts
- **Periodic Systems**: Crystal structures, band calculations
- **Benchmarking**: Extrapolation to complete basis set limit

## 相关概念 / Related Concepts

- **平面波基组**: 周期系统常用
- **赝势**: 减少基组需求
- **基组超位置误差 (BSSE)**: Counterpoise 校正
- **收缩系数**: 优化基组效率
- **角动量**: s, p, d, f 轨道

- **Plane-wave basis**: Common for periodic systems
- **Pseudopotentials**: Reduce basis set requirements
- **Basis Set Superposition Error (BSSE)**: Counterpoise correction
- **Contraction coefficients**: Optimize basis efficiency
- **Angular momentum**: s, p, d, f orbitals

## 来源 / Sources

- CP2K Input Reference: KIND section, BASIS_SET keyword
- /Users/yhm/Desktop/code/cp2k-lsp-enhanced/tests/inputs/He_PBE.inp (line 32)
- Jensen (2007). "Introduction to Computational Chemistry"
- Dunning (1989). "Gaussian basis sets for use in correlated molecular calculations"

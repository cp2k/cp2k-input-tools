# k 点采样 / k-points Sampling

## 定义 / Definition

k 点采样是在倒易空间（布里渊区）中离散积分的方法，用于周期性系统的 DFT 计算。通过在布里渊区中选择一组代表点（k 点网格），将连续的积分问题转化为离散求和。

k-points sampling is a method for discrete integration in reciprocal space (Brillouin zone) for DFT calculations on periodic systems. By selecting a set of representative points (k-point mesh) in the Brillouin zone, continuous integration is converted to discrete summation.

## 核心机制 / Core Mechanism

### 布里渊区和倒易空间 / Brillouin Zone and Reciprocal Space

对于实空间晶格向量 R = n1a1 + n2a2 + n3a3，倒易空间格向量为：For real-space lattice vectors R = n1a1 + n2a2 + n3a3, reciprocal space vectors are:

```
b1 = 2π(a2 × a3)/(a1 · (a2 × a3))
b2 = 2π(a3 × a1)/(a1 · (a2 × a3))
b3 = 2π(a1 × a2)/(a1 · (a2 × a3))
```

k 点定义在第一布里渊区内。

### k 点积分 / k-point Integration

周期系统中的积分变为：Integration in periodic systems becomes:

```
∫_BZ f(k) dk ≈ (1/N_k) Σ_k w_k f(k)
```

其中 w_k 是权重，N_k 是 k 点总数。

Where w_k is weight, N_k is total number of k-points.

### k 点网格类型 / k-point Mesh Types

| 网格类型 / Mesh Type | 描述 / Description | 应用 / Applications |
|---------------------|--------------------|---------------------|
| Monkhorst-Pack | 均匀网格，包含 Γ 点 | 金属，一般系统 |
| Gamma-centered | Γ 点为中心，对称优化 | 半导体，绝缘体 |
| 自定义路径 | 沿高对称线 | 能带结构图 |

| Mesh Type | Description | Applications |
|------------|-------------|---------------------|
| Monkhorst-Pack | Uniform mesh, includes Γ point | Metals, general systems |
| Gamma-centered | Γ-centered, symmetry optimized | Semiconductors, insulators |
| Custom path | Along high-symmetry lines | Band structure plots |

### 网格密度选择 / Mesh Density Selection

**粗略指南**: Rough guideline:

```
n_k × a ≈ 20-30 Å      (绝缘体/绝缘体 insulators)
n_k × a ≈ 40-60 Å      (金属/metals)
```

其中 n_k 是每个方向的网格点数，a 是晶格常数。

Where n_k is grid points per direction, a is lattice constant.

**CP2K 设置**: CP2K settings:

```cp2k
&FORCE_EVAL
  &DFT
    &KPOINTS
      SCHEME MONKHORST_PACK
      GRID 4 4 4           # 4×4×4 网格
      # 或
      SCHEME GAMMA        # 仅 Γ 点
      # 或
      FULL_GRID .FALSE.   # 利用对称性
    &END KPOINTS
  &END DFT
&END FORCE_EVAL
```

### 对称性和约化 / Symmetry and Reduction

利用晶体对称性可以约化 k 点数：Crystal symmetry can reduce k-points:

- **不可约布里渊区 (IBZ)**: 对称不等效的 k 点集合
- **权重因子**: 考虑对称性多重数
- **节省**: 通常可减少 10-100 倍计算量

- **Irreducible Brillouin Zone (IBZ)**: Set of symmetry-inequivalent k-points
- **Weight factors**: Account for symmetry multiplicity
- **Savings**: Typically 10-100x reduction

### 金属特殊处理 / Special Treatment for Metals

金属需要更密的 k 点网格，因为：Metals require denser k-point meshes because:

- 费米面附近态密度变化剧烈
- 可能需要 smearing 方法
- 部分占据轨道

- Steep density of states near Fermi level
- May require smearing methods
- Partially occupied orbitals

```cp2k
&KPOINTS
  SCHEME MONKHORST_PACK
  GRID 8 8 8               # 金属需要更密网格
  FULL_GRID .TRUE.
&END KPOINTS

&SCF
  SMEAR METHOD FERMI_DIRAC
  SMEAR_ELECTRONIC_TEMPERATURE 300.0
&END SCF
```

## 应用场景 / Applications

- **能带结构**: 沿高对称线计算能量色散
- **态密度 (DOS)**: k 空间积分得到电子态分布
- **总能量收敛**: 确保能量精度 < 1 meV/atom
- **金属系统**: 费米面精确描述
- **应力计算**: 需要更密网格

- **Band Structure**: Energy dispersion along high-symmetry lines
- **Density of States (DOS)**: Electronic state distribution from k-space integration
- **Total Energy Convergence**: Ensure energy accuracy < 1 meV/atom
- **Metallic Systems**: Precise Fermi surface description
- **Stress Calculations**: Require denser meshes

## 相关概念 / Related Concepts

- **布里渊区**: 倒易空间的单胞
- **倒易空间**: 正空间的傅里叶对偶
- **周期性边界条件**: 实空间周期性对应 k 点离散化
- **Bloch 定理**: 周期势下的电子态
- **费米面**: 金属的 k 空间等能面

- **Brillouin Zone**: Unit cell of reciprocal space
- **Reciprocal Space**: Fourier dual of real space
- **Periodic Boundary Conditions**: Real-space periodicity ↔ k-point discretization
- **Bloch Theorem**: Electronic states in periodic potentials
- **Fermi Surface**: Constant-energy surface in k-space for metals

## 来源 / Sources

- CP2K Input Reference: KPOINTS section
- Monkhorst & Pack (1976). "Special points for Brillouin-zone integrations"
- Methfessel & Paxton (1989). "High-precision sampling for Brillouin-zone integration"
- Martin (2004). "Electronic Structure: Basic Theory and Practical Methods"

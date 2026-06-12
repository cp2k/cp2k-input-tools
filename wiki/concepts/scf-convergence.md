# SCF 收敛 / SCF Convergence

## 定义 / Definition

自洽场（Self-Consistent Field, SCF）是 DFT 计算的核心迭代过程，通过循环求解 Kohn-Sham 方程直到电子密度和势能自洽。SCF 收敛是指迭代达到稳定解的过程，收敛准则决定计算精度和效率。

Self-Consistent Field (SCF) is the core iterative process in DFT calculations, cyclically solving Kohn-Sham equations until electron density and potential are self-consistent. SCF convergence refers to the process of reaching a stable solution, with convergence criteria determining accuracy and efficiency.

## 核心机制 / Core Mechanism

### SCF 迭代循环 / SCF Iteration Cycle

```
初始密度 → 构造势 → 求解 KS → 新密度 → 检查收敛 → (循环)
         ↑                              ↓
         ←—— 未收敛 ——←————— 混合密度 ←—————┘
```

1. **初始猜测**: 从超叠加原子或 previous step
2. **构造有效势**: V_eff = V_ext + V_H + V_xc
3. **求解 KS 方程**: 对角化 Hamiltonian
4. **计算新密度**: 占据轨道构建
5. **混合**: 新旧密度混合以稳定迭代
6. **收敛检查**: 密度/能量变化 < 阈值

1. **Initial guess**: From superposition of atoms or previous step
2. **Construct effective potential**: V_eff = V_ext + V_H + V_xc
3. **Solve KS equations**: Diagonalize Hamiltonian
4. **Calculate new density**: From occupied orbitals
5. **Mixing**: Blend new and old densities for stability
6. **Convergence check**: Density/energy change < threshold

### 混合方案 / Mixing Schemes

| 方案 / Scheme | 公式 / Formula | 特点 / Characteristics |
|---------------|----------------|------------------------|
| 简单混合 | ρ_new = αρ_in + (1-α)ρ_out | 简单，但收敛慢 |
| Pulay (DIIS) | 最小化误差泛函 | 快速收敛，最常用 |
| Broyden | 秩-2 更新 | 适用于困难系统 |
| Kerker | 长程振荡阻尼 | 金属系统 |

| Scheme | Formula | Characteristics |
|--------|----------|------------------------|
| Simple mixing | ρ_new = αρ_in + (1-α)ρ_out | Simple but slow |
| Pulay (DIIS) | Minimize error functional | Fast convergence, most common |
| Broyden | Rank-2 update | Difficult systems |
| Kerker | Long-range oscillation damping | Metallic systems |

### DIIS (Direct Inversion in Iterative Subspace)

DIIS 外推公式：DIIS extrapolation formula:

```
ρ_new = Σ_i c_i ρ_i
```

系数 c_i 通过最小化误差得到：Coefficients c_i obtained by minimizing error:

```
min Σ_i,j c_i c_j e_i · e_j
```

### 收敛准则 / Convergence Criteria

| 准则 / Criterion | 典型值 / Typical Value | 描述 / Description |
|------------------|------------------------|--------------------|
| EPS_SCF | 1.0E-6 ~ 1.0E-8 | 总能量变化 |
| EPS_POT | 1.0E-5 ~ 1.0E-7 | 势能变化 |
| EPS_DIIS | 0.01 ~ 0.1 | DIIS 误差阈值 |
| MAX_SCF | 30 ~ 100 | 最大迭代步数 |

| Criterion | Typical Value | Description |
|-----------|----------------|--------------------|
| EPS_SCF | 1.0E-6 ~ 1.0E-8 | Total energy change |
| EPS_POT | 1.0E-5 ~ 1.0E-7 | Potential change |
| EPS_DIIS | 0.01 ~ 0.1 | DIIS error threshold |
| MAX_SCF | 30 ~ 100 | Maximum iterations |

### CP2K SCF 设置 / CP2K SCF Settings

```cp2k
&SCF
  EPS_SCF 1.0E-6             # 能量收敛阈值
  MAX_SCF 30                 # 最大迭代
  SCF_GUESS ATOMIC           # 初始猜测
  &DIIS
    EPS_DIIS 0.1             # DIIS 误差阈值
    MAX_DIIS 4               # DIIS 历史
  &END DIIS
  &MIXING
    METHOD 0.5               # 简单混合系数
    # 或
    METHOD PULAY             # DIIS 混合
    ALPHA 0.4                # Pulay 系数
    BETA 0.1                 # 预条件
  &END MIXING
  &SMEAR
    METHOD FERMI_DIRAC
    Electronic_Temperature 300.0
  &END SMEAR
&END SCF
```

### 收敛困难处理 / Handling Convergence Difficulties

| 问题 / Problem | 解决方案 / Solution |
|----------------|--------------------|
| 振荡 | 降低混合系数，启用 DIIS |
| 慢收敛 | 增加 MAX_DIIS，使用更好的初始猜测 |
| 不收敛 | 检查几何构型，使用 smearing |
| 金属系统 | Kerker 混合 + smearing |
| 大系统 | 子空间旋转，分块处理 |

| Problem | Solution |
|----------|----------|
| Oscillation | Reduce mixing coefficient, enable DIIS |
| Slow convergence | Increase MAX_DIIS, better initial guess |
| No convergence | Check geometry, use smearing |
| Metallic systems | Kerker mixing + smearing |
| Large systems | Subspace rotation, block processing |

## 应用场景 / Applications

- **单点计算**: 确定给定结构的电子结构
- **几何优化中的 SCF**: 每步几何需要 SCF 收敛
- **分子动力学**: Born-Oppenheimer MD 每步需要 SCF
- **响应性质**: 极化率，NMR 屏蔽需要高收敛
- **激发态**: TD-DFT 需要良好基态收敛

- **Single-Point Calculations**: Determine electronic structure of given geometry
- **SCF in Geometry Optimization**: Each geometry step requires SCF convergence
- **Molecular Dynamics**: Born-Oppenheimer MD needs SCF per step
- **Response Properties**: Polarizability, NMR shielding need high convergence
- **Excited States**: TD-DFT requires good ground-state convergence

## 相关概念 / Related Concepts

- **DFT**: SCF 是 DFT 求解的核心
- **Kohn-Sham 方程**: SCF 求解的对象
- **电子密度**: SCF 迭代的基本变量
- **对角化**: KS 方程求解的数值方法
- **Smearing**: 金属系统的辅助技术

- **DFT**: SCF is core of DFT solution
- **Kohn-Sham Equations**: What SCF solves
- **Electron Density**: Basic variable in SCF iteration
- **Diagonalization**: Numerical method for KS solution
- **Smearing**: Auxiliary technique for metallic systems

## 来源 / Sources

- CP2K Input Reference: SCF section, DIIS subsection
- /Users/yhm/Desktop/code/cp2k-lsp-enhanced/tests/inputs/He_PBE.inp (lines 13-19)
- Pulay (1980). "Convergence acceleration of iterative sequences"
- Kresse & Furthmüller (1996). "Efficient iterative schemes for ab initio total-energy calculations"

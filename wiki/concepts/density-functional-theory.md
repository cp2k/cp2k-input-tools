# 密度泛函理论 / Density Functional Theory

## 定义 / Definition

密度泛函理论（Density Functional Theory, DFT）是一种量子力学方法，用于研究多电子系统的电子结构。其核心思想是系统的所有性质都可以由电子密度唯一确定，而非依赖于多电子波函数。

Density Functional Theory (DFT) is a quantum mechanical approach for studying the electronic structure of many-body systems. Its core principle is that all properties of a system are uniquely determined by the electron density, rather than depending on the many-electron wave function.

## 核心机制 / Core Mechanism

### Hohenberg-Kohn 定理 / Hohenberg-Kohn Theorems

1. **第一定理**：外势场 V(r) 是电子密度的唯一泛函（除常数外），因此电子密度确定系统的所有性质。
2. **第二定理**：存在一个能量泛函，其极小值对应于基态电子密度和能量。

1. **First Theorem**: The external potential V(r) is a unique functional of the electron density (up to a constant), therefore the electron density determines all properties of the system.
2. **Second Theorem**: There exists an energy functional whose minimum corresponds to the ground-state electron density and energy.

### Kohn-Sham 方程 / Kohn-Sham Equations

Kohn-Sham 方法将相互作用的多电子系统映射到无相互作用的参考系统：

The Kohn-Sham method maps the interacting many-electron system to a non-interacting reference system:

```
[-½∇² + V_eff(r)] φ_i(r) = ε_i φ_i(r)
```

其中有效势包含：Where the effective potential contains:
- 外势 V_ext(r) - External potential
- Hartree 势 V_H(r) - Hartree potential
- 交换-相关势 V_xc(r) - Exchange-correlation potential

### 交换-相关泛函 / Exchange-Correlation Functionals

| 泛函类型 / Type | 示例 / Examples | 特点 / Characteristics |
|-----------------|-----------------|------------------------|
| LDA (局域密度近似) | SVWN | 均匀电子气模型，适用于简单金属 |
| GGA (广义梯度近似) | PBE, BLYP | 包含密度梯度，适用于分子和固体 |
| Meta-GGA | TPSS, M06-L | 包含动能密度，更高精度 |
| Hybrid (杂化) | B3LYP, PBE0 | 混合 Hartree-Fock 交换，高精度 |

| Type | Examples | Characteristics |
|------|-----------|-------------------|
| LDA (Local Density Approx.) | SVWN | Uniform electron gas, simple metals |
| GGA (Generalized Gradient Approx.) | PBE, BLYP | Includes density gradient, molecules and solids |
| Meta-GGA | TPSS, M06-L | Includes kinetic energy density, higher accuracy |
| Hybrid | B3LYP, PBE0 | Mixed Hartree-Fock exchange, high accuracy |

## 应用场景 / Applications

- **分子结构优化**：确定平衡几何构型和键长
- **能量计算**：计算反应能、结合能、电离能
- **光谱预测**：预测红外、拉曼、UV-Vis 光谱
- **材料模拟**：晶体结构、能带结构、态密度
- **化学反应**：反应路径、过渡态、势能面

- **Molecular Structure Optimization**: Determine equilibrium geometry and bond lengths
- **Energy Calculations**: Reaction energies, binding energies, ionization potentials
- **Spectroscopy Prediction**: IR, Raman, UV-Vis spectra
- **Materials Simulation**: Crystal structures, band structures, density of states
- **Chemical Reactions**: Reaction pathways, transition states, potential energy surfaces

## 相关概念 / Related Concepts

- **SCF (Self-Consistent Field)**: 自洽场迭代求解 Kohn-Sham 方程
- **基组 (Basis Sets)**: 展开轨道的数学基函数
- **赝势 (Pseudopotentials)**: 有效描述核心电子
- **周期性边界条件**: 用于晶体和体相材料模拟
- **k 点采样**: 倒易空间积分

- **SCF (Self-Consistent Field)**: Iterative solution of Kohn-Sham equations
- **Basis Sets**: Mathematical basis functions for orbital expansion
- **Pseudopotentials**: Effective description of core electrons
- **Periodic Boundary Conditions**: For crystals and bulk materials
- **k-points Sampling**: Reciprocal space integration

## 来源 / Sources

- CP2K Input Reference: DFT section, XC functionals
- /Users/yhm/Desktop/code/cp2k-lsp-enhanced/tests/inputs/He_PBE.inp
- Hohenberg-Kohn (1964), Kohn-Sham (1965) original papers
- Burke, K. (2012). "Perspective on density functional theory." J. Chem. Phys.

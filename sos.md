# Structural Operational Semantics

See slides Lecture 8 for further details.

Hint: Use inline latex math for GitLab.

## Conditions

### If then

```math
\frac{\langle E,\; s \rangle \Rightarrow \textbf{true}}{\langle \textbf{if}\; E\; \textbf{do} B,\; s \rangle \rightarrow \langle B,\; s \rangle} 
```

```math
\frac{\langle E,\; s \rangle \Rightarrow \textbf{false}}{\langle \textbf{if}\; E\; B,\; s \rangle \rightarrow \langle \textbf{()},\; s \rangle}
```

### If then else

```math
\frac{\langle E,\; s \rangle \Rightarrow \textbf{true}}{\langle \textbf{if}\; E~ \textbf{do} B_1\; \textbf{else}\; B_2,\; s \rangle \rightarrow \langle B_1,\; s \rangle} 
```

```math
\frac{\langle E,\; s \rangle \Rightarrow \textbf{false}}{\langle \textbf{if}\; E~ \textbf{do} B_1\; \textbf{else}\; B_2,\; s \rangle \rightarrow \langle B_2,\; s \rangle}
```

### While

```math
\frac{\langle E,\; s \rangle \Rightarrow \textbf{true}}{\langle \textbf{while}\; E\; \textbf{do}~ C,\; s \rangle \rightarrow \langle C;\; \textbf{while}\; B\; \textbf{do}~ C,\; s \rangle}
```

```math
\frac{\langle E,\; s \rangle \Rightarrow \textbf{false}}{\langle \textbf{while}\; E\; \textbf{do}~ C,\; s \rangle \rightarrow s}
```

## Operations

### Binary Operators (addition)

```math
\frac{\langle E_1,\; s \rangle \Rightarrow i_1 \in \mathbb{N} \quad \langle E_2,\; s \rangle \Rightarrow i_2 \in \mathbb{N}}{\langle E_1 + E_2,\; s \rangle \Rightarrow i_1 + i_2}
```

### Unary Operator (Not)

```math
\frac{\langle E,\; s \rangle \Rightarrow \textbf{true}}{\langle !E,\; s \rangle \Rightarrow \textbf{false}}
```

```math
\frac{\langle E,\; s \rangle \Rightarrow \textbf{false}}{\langle !E,\; s \rangle \Rightarrow \textbf{true}}
```

## Functions

### Functiondeclaration

```math
\frac{}{(fn \: id(p_0, \ldots, p_n)S,s) \Rightarrow s[id(p_0, \ldots, p_n) S/id]}
```

### Function Call

```math
\frac{(id, s) \Rightarrow (id(p0, \ldots, pn) S, s) } {(id (a0, \ldots, an) S, s) \Rightarrow (v,s)}
```
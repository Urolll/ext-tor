# ext-tor

## Module Theory

### Definition (left R-module)

Let $R$ be a ring with multiplicative identity $1_R$. A *left R-module* $M$ is an abelian group $(M, +)$ along with an operator 
$$
\circ : R \times M \to M
$$ 
such that $\forall r, s \in R$ and $\forall m, n \in M$, the following properties hold:

$$
r \circ (m + n) = r \circ m + r \circ n \in M
$$
$$
(r + s) \circ m = r \circ m + s \circ m \in M
$$
$$
(rs) \circ m = r \circ (sm) \in M
$$
$$
\text{If } 1_R \text{ exists, } 1_R \circ m = m \in M
$$

An R-module is a generalization of a vector space. Unlike a vector space, the scalar from the ring does not need to be invertible. In fact, if $R$ happens to be a field, then $M$ is just a vector space.

### Definition (Exact Sequence)

An *exact sequence* is a sequence of morphisms between objects (in this case R-modules) where the image of one morphism equals the kernel of the subsequent morphism.

For modules $M_0, M_1, M_2, \dots, M_k$ over a ring $R$, a sequence of left R-module homomorphisms: 

$$
\cdots \longrightarrow M_{n-1} \xrightarrow{f_{n-1}} M_n \xrightarrow{f_n} M_{n+1} \longrightarrow \cdots
$$

is exact at $M_n$ if:

$$
\operatorname{im}(f_{n-1}) = \ker(f_n)
$$

### Definition (Short Exact Sequence)

A *short exact sequence* is a special case of an exact sequence. Given that $A, B, C$ are R-modules and 0 is the zero module (an R-module that only contains 0):

$$
0 \to A \xrightarrow{f} B \xrightarrow{g} C \to 0
$$

is a short exact sequence if $f$ is injective, $g$ is surjective, and the sequence is exact.

### Definition (Projective Module)

A module $M$ over a ring $R$ is *projective* if it satisfies the "lifting property": 

If there exists a surjective homomorphism $f: M' \twoheadrightarrow M''$ and a homomorphism $g: M \to M''$, then $\exists h: M \to M'$ such that $f \circ h = g$.


### Definition (Injective Module)

A module $M$ over a ring $R$ is injective if it has the "extension property": 

If there exists an injective homomorphism $f: M' \hookrightarrow M''$ and a homomorphism $g: M' \to M$, then $\exists h: M'' \to M$ such that $h \circ f = g$.



## Chain Complexes

### Definition (Chain Complex)

A *chain complex* is a sequence of modules called *chain groups* denoted $C$ with a homomorphism between consecutive chain groups called *boundary maps* denoted $\partial$:

$$
\cdots \xrightarrow{\partial_{n+1}} C_{n} \xrightarrow{\partial_{n}} C_{n-1} \xrightarrow{\partial_{n-1}} \cdots
$$

where $\partial_{n-1} \circ \partial_{n} = 0$ and $\operatorname{im}(\partial_{n+1}) \subseteq \ker(\partial_{n})$.

### Definition (Homology)

Let $R$ be a ring. Let $C_\bullet = (C_n, \partial_n)$ be a chain complex of R-modules. The nth homology group of $C_\bullet$ is the quotient module:

$$
H_n(C_\bullet) = \frac{\ker(\partial_n)}{\operatorname{im}(\partial_{n+1})}
$$

which measures how far $\partial_n$ is from being exact.

#### Proposition

If $H_n(C_\bullet) = 0$, then $C_n$ is exact.

Since $H_n(C_\bullet) = 0$, this means 
$$
\frac{\ker(\partial_n)}{\operatorname{im}(\partial_{n+1})} = 0.
$$
Since the quotient module is trivial, $\ker(\partial_n) = \operatorname{im}(\partial_{n+1})$, which implies that the image of the boundary map at $n+1$ equals the kernel of the boundary map at $n$, so the boundary map at $n$ must be exact.

### Definition (Cochain Complex)

A *cochain complex* is a dual of a chain complex, which is a sequence of modules $C^n$ called *degree n cochains* with a homomorphism between cochains called a *coboundary map* denoted $\partial^n: C^n \to C^{n+1}$:

$$
\cdots \xrightarrow{\partial^{n-1}} C^{n} \xrightarrow{\partial^{n}} C^{n+1} \xrightarrow{\partial^{n+1}} \cdots
$$

where $\partial^{n+1} \circ \partial^n = 0$.

### Definition (Cohomology)

Let $R$ be a ring. Let $C^\bullet = (C^n, \partial^n)$ be a cochain complex of R-modules. The *cohomology at degree n* of $C^\bullet$ is the quotient module:

$$
H^n(C^\bullet) = \frac{\ker(\partial^n)}{\operatorname{im}(\partial^{n-1})}
$$

## Hom Sets and Hom Functors

### Definition (Hom Set)

A *Hom set* denoted $\operatorname{Hom}_{\mathcal{C}}(A, B)$ is a set of morphisms between two objects $A$ and $B$ in a category $\mathcal{C}$.

$$
\operatorname{Hom}_{\mathcal{C}}(A, B) = \{ f: A \to B \mid f \text{ is a morphism in } \mathcal{C} \}
$$

### Definition (Hom Functor)

A *Hom functor* is a functor that constructs Hom sets for all objects in the category $\mathcal{C}$. A Hom functor could either be covariant or contravariant.

### Definition (Covariant Hom Functor)

A *covariant Hom functor* denoted:

$$
\operatorname{Hom}(A, -): \mathcal{C} \to \mathbf{Set}
$$

is a functor where each object $B \in \mathcal{C}$ is sent to the Hom set $\operatorname{Hom}_{\mathcal{C}}(A, B)$ and each morphism $f: B \to C$ is sent to the morphism 

$$
\operatorname{Hom}(A, f): \operatorname{Hom}_{\mathcal{C}}(A, B) \to \operatorname{Hom}_{\mathcal{C}}(A, C)
$$ 

where $\psi \mapsto f \circ \psi$, for all $\psi \in \operatorname{Hom}_{\mathcal{C}}(A, B)$.

The symbol $\mathbf{Set}$ denotes the category of sets.

### Definition (Contravariant Hom Functor)

A *contravariant Hom functor* denoted: 

$$
\operatorname{Hom}(-, A): \mathcal{C} \to \mathbf{Set}
$$

is a functor where each object $B \in \mathcal{C}$ is sent to the Hom set $\operatorname{Hom}_{\mathcal{C}}(B, A)$ and each morphism $f: B \to C$ is sent to the morphism 

$$
\operatorname{Hom}(f, A): \operatorname{Hom}_{\mathcal{C}}(C, A) \to \operatorname{Hom}_{\mathcal{C}}(B, A)
$$ 

where $\psi \mapsto \psi \circ f$, for all $\psi \in \operatorname{Hom}_{\mathcal{C}}(C, A)$.

## The Construction of Ab

### Definition (Abelian Category)

A category $\mathcal{C}$ is *abelian* (denoted $\mathbf{Ab}$) if:

- There is an object 0 that is both initial and terminal.
- Every morphism $f: A \to B$ has a kernel and cokernel.
- Every monomorphism is the kernel of some morphism.
- Every epimorphism is the cokernel of some morphism.
- Finite direct sums and products exist and are isomorphic.

## Tensor Product on Vector Spaces

### Definition (Tensor Product on Vector Spaces)

Let $V$ and $W$ be vector spaces over a field $F$. Let $v_1, v_2 \in V$ and $w_1, w_2 \in W$. Also, let $\xi \in F$.

The tensor product $\otimes: V \times W \to V \otimes W$ produces a vector space with the bilinearity properties:

1. $(v_1 + v_2) \otimes w_1 = (v_1 \otimes w_1) + (v_2 \otimes w_1)$
2. $v_1 \otimes (w_1 + w_2) = (v_1 \otimes w_1) + (v_1 \otimes w_2)$
3. $(\xi v_1) \otimes w_1 = v_1 \otimes (\xi w_1) = \xi(v_1 \otimes w_1)$

Each pair from the cartesian product $V \times W$ of the form $(v, w)$ where $v \in V$, $w \in W$ gets mapped to the form $v \otimes w \in V \otimes W$. These elements are called the *elementary tensors* of $V \otimes W$ and they span the entirety of $V \otimes W$.

## Tensor Product on Modules

### Definition (Tensor Product on Modules)

Let $M$ and $N$ be R-modules over a ring $R$. Let $m_1, m_2 \in M$ and $n_1, n_2 \in N$. Also, let $\xi \in R$.

The tensor product $\otimes_R: M \times N \to M \otimes_R N$ produces an R-module with the bilinearity properties:

1. $(n_1 + n_2) \otimes m_1 = (n_1 \otimes m_1) + (n_2 \otimes m_1)$
2. $n_1 \otimes (m_1 + m_2) = (n_1 \otimes m_1) + (n_1 \otimes m_2)$
3. $(\xi n_1) \otimes m_1 = n_1 \otimes (\xi m_1) = \xi(n_1 \otimes m_1)$

#### Proposition (Universal Property of Tensor Product on Modules)

Let $f: M \times N \to P$ be a bilinear map where the codomain is an R-module $P$. There must exist a unique map $\tilde{f}: M \otimes_R N \to P$ such that for all $m \in M$, $n \in N$:

$$
f(m, n) = \tilde{f}(m \otimes_R n)
$$

## Projective Resolution

### Definition (Projective Resolution)

A *projective resolution of $M$* is a chain complex ([Chain Complex](#chain-complexes)) of projective modules ([Projective Module](#definition-projective-module)) that gets closer to $M$ over time. More formally,

$$
\cdots \xrightarrow{\partial_{3}} C_{2} \xrightarrow{\partial_{2}} C_{1} \xrightarrow{\partial_{1}} C_0 \xrightarrow{\partial_0} M \to 0 
$$

where each $C_i$ is a projective module and each $\partial_i$ is a module homomorphism satisfying $\operatorname{im}(\partial_i) = \ker(\partial_{i-1})$.

## Injective Resolution

### Definition (Injective Resolution)

An injective resolution of $M$ is an exact sequence that gets closer to $M$ over time. More formally,

$$
\cdots \to I^2 \to I^1 \to I^0 \to M \to 0
$$

is an injective resolution where each $I^n$ is an injective module ([Injective Module](#definition-injective-module)) and the sequence is exact.

## Exact Functor

### Definition (Exact Functor)

A functor $F: \mathcal{C} \to \mathcal{D}$ is *exact* if for any short exact sequence in the category $\mathcal{C}$, the exactness of the sequence is preserved in $\mathcal{D}$. So for a short exact sequence:

$$
0 \to A \xrightarrow{f} B \xrightarrow{g} C \to 0
$$

the exact functor $F$ would map the sequence to:

$$
0 \to F(A) \xrightarrow{F(f)} F(B) \xrightarrow{F(g)} F(C) \to 0
$$

which must also be a short exact sequence in $\mathcal{D}$. Thus, $F(f)$ is injective, $F(g)$ is surjective, and $\operatorname{im}(F(f)) = \ker(F(g))$.

### Definition (Left Exact Functor)

A functor $F: \mathcal{C} \to \mathcal{D}$ is *left exact* if it preserves the exactness at the beginning of any short exact sequence in $\mathcal{C}$.

The sequence:

$$
0 \to A \xrightarrow{f} B \xrightarrow{g} C \to 0
$$

would map to:

$$
0 \to F(A) \xrightarrow{F(f)} F(B) \xrightarrow{F(g)} F(C)
$$

which is exact at $F(A)$ and $F(B)$ in $\mathcal{D}$. So, $F(f)$ is injective and $\operatorname{im}(F(f)) = \ker(F(g))$. This is associated with preserving injectivity and the kernel.

### Definition (Right Exact Functor)

A functor $F: \mathcal{C} \to \mathcal{D}$ is *right exact* if it preserves the exactness at the end of any short exact sequence in $\mathcal{C}$.

The sequence:

$$
0 \to A \xrightarrow{f} B \xrightarrow{g} C \to 0
$$

would map to:

$$
F(A) \xrightarrow{F(f)} F(B) \xrightarrow{F(g)} F(C) \to 0
$$

which is exact at $F(B)$ and $F(C)$. So, $F(g)$ is surjective and $\operatorname{im}(F(f)) = \ker(F(g))$. This is associated with preserving surjectivity and the cokernel.

## Derived Functor

### Definition (Left Derived Functor)

Let $\mathcal{F}: \mathbf{Ab} \to \mathbf{Ab}$ be a left exact functor. The *left derived functor* of $\mathcal{F}$ denoted $L^n\mathcal{F}$ for an object $M \in \mathbf{Ab}$ is defined by:

$$
L^n \mathcal{F}(M) := H^n(\mathcal{F}(P_\bullet))
$$

In simple terms:
1. Take a projective resolution of $M$.
2. Apply the functor $\mathcal{F}$ to the projective resolution (this turns it into a cochain complex).
3. Compute the homology of $\mathcal{F}(P_\bullet)$ at degree $n$.

### Definition (Right Derived Functor)

Let $\mathcal{F}: \mathbf{Ab} \to \mathbf{Ab}$ be a right exact functor. The *right derived functor* of $\mathcal{F}$ denoted $L^n\mathcal{F}$ for an object $M \in \mathbf{Ab}$ is defined by:

$$
L^n \mathcal{F}(M) := H^n(\mathcal{F}(I^\bullet))
$$

In simple terms:
1. Take an injective resolution of $M$.
2. Apply the functor $\mathcal{F}$ to the injective resolution (this keeps it as a cochain complex).
3. Compute the homology of $\mathcal{F}(I^\bullet)$ at degree $n$.

## Definition of the Ext Functor

Let $M$ and $N$ be R-modules. The *Ext functor* is defined as the nth right derived functor of the Hom functor. That is, if 
$$
P_\bullet \to M
$$
is a projective resolution of $M$, then the Ext groups are defined as:
$$
\operatorname{Ext}_R^n(M, N) := H^n(\operatorname{Hom}_R(P_\bullet, N))
$$
This functor measures the extent to which the Hom functor fails to be exact.

## Definition of the Tor Functor

Let $M$ and $N$ be R-modules. The *Tor functor* is defined as the nth left derived functor of the tensor product functor. That is, if 
$$
P_\bullet \to M
$$
is a projective resolution of $M$, then the Tor groups are defined as:
$$
\operatorname{Tor}_n^R(M, N) := H_n(P_\bullet \otimes_R N)
$$
This functor measures the failure of the tensor product to be exact.

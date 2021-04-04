
### 3.7. Reference

#### 3.7.1. Figure Reference

As you can see in figure \ref{fig:fig1} -\> GOOGLE

```{r fig1, echo=FALSE, fig.cap="Visualization of the adjusted prices of the Alphabet Inc Class A Stock."}
GOOGL <- getSymbols("GOOGL", auto.assign=F)
g.adj <- GOOGL[,6]
par(mfrow=c(1,1))
plot(g.adj, main="Adjusted Prices ~ Google")
```

\newpage

#### 3.7.2. Equation Reference

GARCH-formula can be seen in \ref{eq:garch}

```{=tex}
\begin{align} \label{eq:garch}
\epsilon_{t} &= \mathrm{log}(x_{t})-\mathrm{log}(x_{t-1}) \nonumber \\
\epsilon_{t} &= \sigma_{t}u_{t} \\
\sigma_{t}^{2} &=c \sigma^{2}+\sum_{j=1}^{n}\alpha_{j}\sigma_{t-j}^{2}+\sum_{k=1}^{m}\beta_{k}\epsilon_{t-k}^{2} \nonumber
\end{align}
```
\newpage

#### 3.7.3. Table Reference

In table \ref{tab:coeftable} you can see the flexerino of the coefficients.

```{r coeftable, echo=FALSE, message=FALSE}
g.adj.lr <- na.exclude(diff(log(g.adj)))
g.subset.lr <- head(g.adj.lr, length(g.adj.lr)-100)
y.garch_11 <- garchFit(~garch(1,1), data=g.subset.lr, delta=2, include.delta=F, 
                       include.mean=F, trace=F)
r1 <- y.garch_11@fit$matcoef[,1]
r2 <- y.garch_11@fit$matcoef[,2]
r3 <- y.garch_11@fit$matcoef[,4]
paras <- data.frame(r1, r2, r3)
rownames(paras) <- c("$\\omega$", "$\\alpha_{1}$", "$\\beta_{1}$")
colnames(paras) <- c("Estimate", "Std. Error", "p-Value")
kable(paras, "latex", escape=F, booktabs = T, linesep="", caption="Coefficients GARCH(1,1).", digits=20)
```

> > > > > > > \newpage

#### 3.7.4. Section Reference {#sec-ref}

Here we can see a wild section, which will reference to itself: [2.7.4.](#sec-ref)
  
  Or reference to the Bitcoin-section [2.3.](#bitcoin)
    
    \newpage
    
    #### 3.7.5. Literature Reference
    
    Add bibliography reference in the `.bib`-file in the add folder.
    
    Here I make a reference to the original bitcoinpaper [@bitcoin] or to the specific page on the NN financial trading paper [@nnfin, pp. 6-8].
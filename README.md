# ESG and SBTi
This project was created for an assignment in Asset Pricing and Climate Mitigation at KTH - Royal Institute of Technology. The purpose is to investigate whether companies adhering to high ESG-standards or those with commited and approved SBTi (Science-Based Target Intiative) targets (*green firms*) outperform stocks with lower standards (*brown firms*).

## ESG
In the file [ESG.R](https://github.com/PontusHovb/ESG-SBTI/blob/main/ESG.R) stocks are sorted based on ESG-quartiles (putting the 25% of stocks with highest ESG-scores in top portfolio, next 25% in second best and so forth). Since ESG-scores are relative to companies in the same sector, these portfolio should not in theory be overwighted in specific industries. These four portfolios are then compared to an equally-weighted (EQW) portfolio where all stocks in the dataset are equally-weighted independent of other factors (such as ESG-score, industry and market cap).

<p align="center">
    <img src="https://github.com/PontusHovb/ESG-SBTI/blob/main/Plots/ESG%20-%20Portfolio%20Performance.png" width="400"/>
</p>
<p align="center"><i>Portfolio performance for ESG-quartile portfolios (2011-2022)</i></p>

In absolute performance there seems to be an inverse relationship between ESG-score and financial performance with the worst ESG-portfolio performing best. ESG-scores neither seems to decrease risk (as measured by $`1\%`$-VaR and volatility $`\sigma`$) and the inverse relationship therefore holds true also on a risk-adjusted basis (Sharpe Ratio).

## SBTi
In the file [SBTi.R](https://github.com/PontusHovb/ESG-SBTI/blob/main/SBTi.R) another variable indicating a company's sustainability maturity is used, whether they have approved SBTi-tarrgets or not. Since it is a volountary for companies to have SBTi-targets or not, and since they require companies to have realistic and approved plan to achieve net-zero it is a good indication of their sustainability work.

To test this, two portfolios are created, one containing SBTi-stocks and one with non-SBTi-stocks. When a company in the dataset have their SBTi-targets approved they are moved from non-SBTi portfolio to SBTi-portfolio. With a requirement of minimum 5 stocks in the SBTi-portfolio, the portfolio can first be formed in September 2019. From then until end of 2022, the performance of the two portfolios is almost identical. Performing a regression on excess returns (controlled for risk using CAPM) we can see that SBTi have low or no performance on financial performance for the stocks in the dataset.

<p align="center">
    <img src="https://github.com/PontusHovb/ESG-SBTi/blob/main/Plots/SBTi%20-%20portfolio%20performance%20(selected).png" width="400"/>
</p>
<p align="center"><i>Portfolio performance SBTi and non-SBTi portfolios (2019-2022)</i></p>

## Source
Data is retrieved from Eikon and filtered based on three conditions:
1. Available price data from 2011 to 2022
2. Avaliable ESG-data from 2010 to 2021 (since lagged ESG-scores are used to form portfolios for coming year)
3. Headquarted in the US (to avoid market effects and for risk-free yields and benchmark returns to be valid)

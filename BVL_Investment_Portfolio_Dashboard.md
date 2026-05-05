---
layout: default
title: BVL Investment Portfolio Dashboard
---
<img src="./BVL_Dashboard_preview.jpg.jfif" alt="BVL Dashboard Preview" width="800">

## Bond Vision Limited Investment Portfolio Dashboard

**Tools used:** Power BI | Power Query | DAX

**Description:**  
**Building a Live Investment Fund Data Infrastructure from Scratch**
Power BI | DAX | Data Modelling | Financial Reporting

**Background**
Bond Vision Limited (BVL) is a private investment fund co-founded in 2025 with seven members, investing across Nigerian equities, fixed income, and money market instruments. When the fund launched in October 2025, no transactional or investment data infrastructure existed, only basic membership and subscription records. Everything needed to be built from the ground up, in parallel with the fund's live operation.

**The Challenge**
Designed and build a complete data model and reporting layer capable of tracking fund performance in real time, from raw transaction data through to member-facing insights, with no historical baseline and data growing continuously as the fund made its first investments.

**What I Built**
I architected a nine-table Power BI data model covering the full investment lifecycle:

- **Membership & Subscription tables** — members personal information and unit subscription allocation tracking  
- **Investment & Transaction tables** — instrument-level purchase and valuation records  
- **Cash Flow table** — fund liquidity and movement tracking  
- **Price History table** — a dynamic fact table replacing an initial static NAV table, enabling time-intelligent performance calculations across all instruments  
- **NAV table** — net asset value tracking and performance  
- **Investment Cycle & MMF Yield tables** — enabling insights into investment returns and money market return calculations  

Key DAX measures built includes AUM, NAV per unit, Fund ROI, member-level return attribution, gross dividend income, and a two-tranche withholding tax (WHT) framework for dividend treatment.

**The Outcome**
BVL now operates with a structured Power BI dashboard giving members clear, at-a-glance visibility of fund performance, including AUM movement, NAV per unit, asset allocation, and returns by instrument. The dashboard directly informs monthly member updates and supports evidence-based investment decisions by the fund's management team.

[View Dashboard (PDF)](./BVL_Investment_Dashboard_use.pdf)

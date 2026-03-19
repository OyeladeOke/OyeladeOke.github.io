rm(list = ls())
# Load libraries
library(readxl)
library(dplyr)
library(ggplot2)

### Set WD
setwd("C:/Users/user/Desktop/Portfolios/Analytics/NHS/ONS")

# Import data
data <- read_excel("Prostate_Cancer_UK_Data.xlsx")

# Select only relevant columns
cleaned_data <- data %>%
  select(Condition, Sex, Domain, Group, `Number Of Deaths`, Population, `Age-Standardised Rate Per 100,000 Person Years`) %>%
  rename(
    Ethnic_Group = Group,
    No_of_Death = `Number Of Deaths`,
    ASMRs = `Age-Standardised Rate Per 100,000 Person Years`
  ) %>%
  mutate(
    No_of_Death = as.numeric(gsub(",", "", No_of_Death)),
    Population = as.numeric(gsub(",", "", Population)),
    ASMRs = as.numeric(gsub(",", "", ASMRs))
  )

# Filter only Prostate Cancer (Male)
filtered_data <- cleaned_data %>%
  filter(Condition == "Prostate Cancer", Sex == "Male", Domain == "Ethnic Group") %>%
  mutate(
    Perc_of_Ethnic_Death = (No_of_Death / Population) * 100,
    Perc_Total_Popl = (Population / sum(Population)) * 100
  )

### ------------------------------
### Population Distribution Plot
### ------------------------------
filtered_data$Ethnic_Group <- factor(filtered_data$Ethnic_Group,
                                     levels = filtered_data$Ethnic_Group[order(-filtered_data$Perc_Total_Popl)]
)

ggplot(filtered_data, aes(x = Ethnic_Group, y = Perc_Total_Popl)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = paste0(round(Perc_Total_Popl, 1), "%")),
            vjust = -0.5, color = "black", size = 4, fontface = "bold"
  ) +
  labs(
    title = "Three in four UK males are White British, with minority groups making up smaller shares",
    x = "Ethnic Group", y = "Population Share (%)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    plot.title = element_text(face = "bold", size = 14),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    #panel.grid.minor.y = element_blank(),
    #panel.grid.major.y = element_blank()
)
### ------------------------------
### Mortality by ASMRs Plot
### ------------------------------
# Custom color: darker red for Black Caribbean & Black African
filtered_data <- filtered_data %>%
  mutate(Highlight = ifelse(Ethnic_Group %in% c("Black Caribbean", "Black African"),
                            "Target Ethnic Group", "Others"))

ggplot(filtered_data, aes(x = reorder(Ethnic_Group, -ASMRs), y = ASMRs, fill = Highlight)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(ASMRs, 1)),
            vjust = -0.5, color = "black", size = 4, fontface = "bold"
  ) +
  scale_fill_manual(values = c("Target Ethnic Group" = "darkred", "Others" = "tomato")) +
  labs(
    title = "Black Caribbean & Black African men face the highest prostate cancer mortality rates",
    x = "Ethnic Group", y = "ASMRs (per 100,000)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    plot.title = element_text(face = "bold", size = 14),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    #panel.grid.minor.y = element_blank(),
    #panel.grid.major.y = element_blank()
    legend.position = "none"
  )

### ------------------------------
### Combined Plot: Mortality vs Population
### ------------------------------
ggplot(filtered_data, aes(x = Ethnic_Group)) +
  # Mortality bars
  geom_bar(aes(y = ASMRs, fill = Highlight), stat = "identity") +
  geom_text(aes(y = ASMRs, label = round(ASMRs, 1)),
            vjust = -0.5, color = "black", size = 3.5
  ) +
  
  # Population line
  geom_line(aes(y = Perc_Total_Popl / max(Perc_Total_Popl) * max(ASMRs),
                group = 1, color = "Population %"), size = 1.2) +
  geom_point(aes(y = Perc_Total_Popl / max(Perc_Total_Popl) * max(ASMRs),
                 color = "Population %"), size = 2) +
  
  # Dual axis
  scale_y_continuous(
    name = "ASMRs (per 100,000)",
    sec.axis = sec_axis(~ . * max(filtered_data$Perc_Total_Popl) / max(filtered_data$ASMRs),
                        name = "Population Share (%)")
  ) +
  
  scale_fill_manual(values = c("Target Ethnic Group" = "darkred", "Others" = "tomato")) +
  scale_color_manual(values = c("Population %" = "blue")) +
  
  labs(
    title = "Smaller Black ethnic populations carry a disproportionately higher mortality burden",
    x = "Ethnic Group",
    caption = "Source: ONS, Ethnic differences in prostate cancer mortality"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    plot.title = element_text(face = "bold", size = 14),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    #panel.grid.minor.y = element_blank(),
    #panel.grid.major.y = element_blank()
    legend.position = "top",
    plot.caption = element_text(hjust = 0, face = "italic", size = 9)
  )

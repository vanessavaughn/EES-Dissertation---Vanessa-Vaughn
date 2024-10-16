library(tidyr)
library(ggplot2)
ggplot(Auchencorth_filtered, aes(x=start_time, y=NEE))+geom_point()+theme_minimal()
ggplot(Auch2023, aes(x=start_time, y=H))+geom_point()+theme_minimal()

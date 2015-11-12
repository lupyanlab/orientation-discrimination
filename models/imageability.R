devtools::load_all("orientationdiscrimination")
data(orientation)

library(lme4)
library(dplyr)
library(ggplot2)
 
# Merge pic ratings
# -----------------
pic_ratings <- read.csv("experiment/stimuli/pics/_pic_ratings.csv",
                        stringsAsFactors = FALSE)
orientation <- orientation %>% left_join(pic_ratings)

# Drop outliers
# -------------
orientation <- orientation %>%
  filter(rt < 2000)

# Recode variables
# ----------------
orientation <- orientation %>%
  recode_cue_type %>%
  recode_mask_type

# Fit models
# ----------
# Test if imagebility predicts the size of the masking effect
image_mod <- lmer(rt ~ (valid_v_baseline + invalid_v_baseline) * mask_c + (1|subj_id), data = filter(orientation, exp == "modality_block"))
summary(image_mod)


image_mod <- lmerTest::lmer(rt ~ (cue_l + cue_q) * (visual_v_nomask + auditory_v_nomask) + image_mean + (1|subj_id),
                  data = filter(orientation,
                                exp == "modality_blocked",
                                !(subj_id %in% paste0("ODM", 301:308))))
summary(image_mod)

# Make some plots
# ---------------
ggplot(orientation, aes(x = mask_c, y = rt, color = cue_type)) +
  geom_point() +
  facet_wrap("exp")
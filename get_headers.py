
import pandas as pd
df = pd.read_csv('analysis_data/biome_20251224_034818.csv')
for col in df.columns:
    print(col)

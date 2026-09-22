
Hi Becky, some updates on this

I used your actual land-cover map directly (`ph_baseline_lulc_md5_7f29da.tif`) as per `.ini`file. I built a lookup table translating the 12 land-cover categories into GCN250 (Jaafar et al. 2019), the global CN table I'm using, keyed to the ESA classification. 

I tried a first model and had a preliminary set of results, but had to fix it i was using the wrong DEM (90m). I also found three bugs in the original code. Running an isolated test now.

I was also thinkiing, we still don't have good CN
numbers for mangroves or flooded grasslands/savannas, but TBH for mangroves I think it is not even necessary, there is nothing downstream form them anywa — it's a terminal position, so a wrong CN there can't bias anything routed further down. Flooded savannas are different, i have some ideas but will worry about that later .

Once the new run finishes, I'll send the updated numbers, a pixel-wise comparison against WWF-SIPA baseline, and a report

import csv

import pandas as pd

import random



### Targets
targets = []

cond_to_type = {
    "Telescoping + each": "TeleEach",
    "Non-Telescoping + every": "NonTeleEvery",
    "Non-Telescoping + each": "NonTeleEach",
    "Non-Telescoping + the": "NonTeleThe",
}

yes_no_to_num = {
    "Y": 1,
    "N": 0,
    "":"None"
}


with open("stimuli/target_raw.csv", "r") as f:
    reader = csv.DictReader(f)
    data = list(reader)
for d in data:
    # print(d.keys())
    conditions = ["Telescoping + each", "Non-Telescoping + every", "Non-Telescoping + each", 'Non-Telescoping + the']
    for cond in conditions:
        targets.append(
            {   
                "frame_id": d["frame_id"],
                "condition": cond_to_type[cond],
                "sent": d[cond],
                "comp_q": d["Comprehension Question"].strip() if d["Comprehension Question"] else "None",
                "comp_a": yes_no_to_num[d["Comprehension Question Answer"]],
                "type": "target"
            }
        )
assert len(targets) == 80
pd.DataFrame(targets).to_csv("stimuli/targets.csv", index=False)


### Fillers
with open("stimuli/filler_raw.csv", "r") as f:
    reader = csv.DictReader(f)
    data = list(reader)
fillers = []
for i, d in enumerate(data):
    fillers.append(
        {
            "frame_id": d["frame_id"],
            "condition": f"filler{i}",
            "sent": d["sent"],
            "comp_q": d["comp_q"].strip() if d["comp_q"] else "None",
            "comp_a": yes_no_to_num[d["comp_q_ans"]],
            "type": "filler",
        }
    )
assert len(fillers) == 20
pd.DataFrame(fillers).to_csv("stimuli/fillers.csv", index=False)


### Split targets into 4 versions
per_person_stimuli = [[] for _ in range(4)]

types = ["TeleEach", "NonTeleEvery", "NonTeleEach", "NonTeleThe"]
j = 0

intervals = [(0, 5), (5, 10), (10, 15), (15, 20)]
for i in range(4):
    for j, t in enumerate(types):
        current_targets = [x for x in targets if x["condition"] == t]
        assert len(current_targets) == 20
        per_person_stimuli[i] += current_targets[intervals[j][0]:intervals[j][1]]
    i += 1
    types = types[-1:] + types[:-1]

def strict_shuffle(lst,key):
    while True:
        random.shuffle(lst)
        if all(lst[i][key] != lst[i+1][key] for i in range(len(lst)-1)):
            return lst

for i, p in enumerate(per_person_stimuli):
    # per_person_stimuli[i] = strict_shuffle(p, "condition")
    all_for_person = per_person_stimuli[i] + fillers
    for d in all_for_person:
        d["version_id"] = i
    strict_shuffle(all_for_person, "condition")
    pd.DataFrame(all_for_person).to_csv(f"stimuli/stimuli_{i}.csv", index=False)



# for i in range(rotations):
#     for start, end in intervals:
#         print(start, end)
#     intervals = [intervals[-1]] + intervals[:-1]



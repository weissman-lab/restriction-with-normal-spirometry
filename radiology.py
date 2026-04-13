from transformers import AutoTokenizer, AutoModelForSequenceClassification
from torch.utils.data import DataLoader, Dataset
import torch

import os

import pandas as pd
import multiprocessing
from sklearn.model_selection import train_test_split
from sklearn.metrics import classification_report

os.environ["TOKENIZERS_PARALLELISM"] = "false"


#multiprocessing.set_start_method ('spawn', force=True)

# ILD

model_name = "emilyalsentzer/Bio_ClinicalBERT"
tokenizer = AutoTokenizer.from_pretrained (model_name)
model = AutoModelForSequenceClassification.from_pretrained (model_name, num_labels = 2) 

df = pd.read_csv("../data/ild_labeled.csv")

train_df, test_df = train_test_split (df, test_size = 0.2, random_state = 42)

class RadiologyDataset (Dataset):
    def __init__(self, texts, labels, tokenizer, max_len):
        self.texts = texts.tolist() if isinstance(texts, pd.Series) else texts
        self.labels = labels.tolist() if isinstance(labels, pd.Series) else labels
        self.tokenizer = tokenizer
        self.max_len = max_len

    def __len__(self):
        return len(self.texts)

    def __getitem__(self, item):
        text = str(self.texts[item])
        label = self.labels[item]

        encoding = self.tokenizer.encode_plus(
            text,
            add_special_tokens=True,
            max_length=self.max_len,
            return_token_type_ids=False,
            padding='max_length',
            return_attention_mask=True,
            return_tensors='pt',
            truncation=True
        )

        return {
            'input_ids': encoding['input_ids'].flatten(),
            'attention_mask': encoding['attention_mask'].flatten(),
            'labels': torch.tensor(label, dtype=torch.long)
        }

train_dataset = RadiologyDataset (train_df.text, train_df.label, tokenizer, 256)
test_dataset = RadiologyDataset (test_df.text, test_df.label, tokenizer, 256)

train_dataloader = DataLoader(train_dataset, batch_size = 32, shuffle = True)
test_dataloader = DataLoader(test_dataset, batch_size = 32)

device = torch.device('cuda' if torch.cuda.is_available() else 'cpu')
model.to (device)

optimizer = torch.optim.AdamW (model.parameters (), lr = 2e-5)
epochs = 4

for epoch in range(epochs):
    model.train ()
    total_loss = 0
    for batch in train_dataloader:
        input_ids = batch ['input_ids'].to(device)
        attention_mask = batch ['attention_mask'].to(device)
        labels = batch ['labels'].to(device)

        optimizer.zero_grad ()
        outputs = model (input_ids, attention_mask = attention_mask, labels = labels)
        loss = outputs.loss
        total_loss += loss.item ()
        loss.backward ()
        optimizer.step ()

    print(f"Epoch {epoch+1}/{epochs}, Loss: {total_loss/len(train_dataloader)}")

model.eval ()
predictions = []
actual_labels = []

with torch.no_grad():
    for batch in test_dataloader:
        input_ids = batch['input_ids'].to(device)
        attention_mask = batch['attention_mask'].to(device)
        labels = batch['labels'].to(device)

        outputs = model(input_ids, attention_mask=attention_mask)
        _, preds = torch.max(outputs.logits, dim=1)

        predictions.extend(preds.cpu().tolist())
        actual_labels.extend(labels.cpu().tolist())

print (classification_report (actual_labels, predictions))

#model.save_pretrained ("../models/model_ild")
#tokenizer.save_pretrained ("../models/tokenizer_ild")

# Use model to classify all impressions for the presence or absence of ILD

impressions = pd.read_csv("../data/impressions.csv")

new_texts = impressions ['impression'].tolist()

def classify_impressions (texts, tokenizer, model, device):
    # Create array to store predictions
    predictions = []
    
    # Process each text
    for text in texts:
        inputs = tokenizer(text, return_tensors="pt", truncation=True, padding=True, max_length = 256).to(device)
        
        with torch.no_grad():
            outputs = model(**inputs)
            logits = outputs.logits
            probs = torch.softmax(logits, dim=-1)
        
        # Get prediction
        predicted_class_id = probs.argmax().item()
        #predicted_label = model.config.id2label[predicted_class_id]
        
        # Store result
        #predictions.append(predicted_label)
        predictions.append(predicted_class_id)
    
    # Convert to numpy array for easy R conversion
    #predictions_array = np.array (predictions)
    
    # Return just the predictions array
    #return predictions_array
    return (predictions)

classifications_ild = classify_impressions (new_texts, tokenizer, model, device)



# # COPD
# 
# model_name = "emilyalsentzer/Bio_ClinicalBERT"
# tokenizer = AutoTokenizer.from_pretrained (model_name)
# model = AutoModelForSequenceClassification.from_pretrained (model_name, num_labels = 2) 
# 
# df = pd.read_csv("../data/copd_labeled.csv")
# 
# train_df, test_df = train_test_split (df, test_size = 0.2, random_state = 42)
# 
# class RadiologyDataset (Dataset):
#     def __init__(self, texts, labels, tokenizer, max_len):
#         self.texts = texts.tolist() if isinstance(texts, pd.Series) else texts
#         self.labels = labels.tolist() if isinstance(labels, pd.Series) else labels
#         self.tokenizer = tokenizer
#         self.max_len = max_len
# 
#     def __len__(self):
#         return len(self.texts)
# 
#     def __getitem__(self, item):
#         text = str(self.texts[item])
#         label = self.labels[item]
# 
#         encoding = self.tokenizer.encode_plus(
#             text,
#             add_special_tokens=True,
#             max_length=self.max_len,
#             return_token_type_ids=False,
#             padding='max_length',
#             return_attention_mask=True,
#             return_tensors='pt',
#             truncation=True
#         )
# 
#         return {
#             'input_ids': encoding['input_ids'].flatten(),
#             'attention_mask': encoding['attention_mask'].flatten(),
#             'labels': torch.tensor(label, dtype=torch.long)
#         }
# 
# MAX_LEN = 256
# BATCH_SIZE = 32
# 
# train_dataset = RadiologyDataset (train_df.text, train_df.label, tokenizer, MAX_LEN)
# test_dataset = RadiologyDataset (test_df.text, test_df.label, tokenizer, MAX_LEN)
# 
# train_dataloader = DataLoader(train_dataset, batch_size = BATCH_SIZE, shuffle = True)
# test_dataloader = DataLoader(test_dataset, batch_size = BATCH_SIZE)
# 
# device = torch.device('cuda' if torch.cuda.is_available() else 'cpu')
# model.to(device)
# 
# optimizer = torch.optim.AdamW (model.parameters (), lr = 2e-5)
# epochs = 4
# 
# for epoch in range(epochs):
#     model.train ()
#     total_loss = 0
#     for batch in train_dataloader:
#         input_ids = batch ['input_ids'].to(device)
#         attention_mask = batch ['attention_mask'].to(device)
#         labels = batch ['labels'].to(device)
# 
#         optimizer.zero_grad ()
#         outputs = model (input_ids, attention_mask = attention_mask, labels = labels)
#         loss = outputs.loss
#         total_loss += loss.item ()
#         loss.backward ()
#         optimizer.step ()
# 
#     print(f"Epoch {epoch+1}/{epochs}, Loss: {total_loss/len(train_dataloader)}")
# 
# model.eval ()
# predictions = []
# actual_labels = []
# 
# with torch.no_grad():
#     for batch in test_dataloader:
#         input_ids = batch['input_ids'].to(device)
#         attention_mask = batch['attention_mask'].to(device)
#         labels = batch['labels'].to(device)
# 
#         outputs = model(input_ids, attention_mask=attention_mask)
#         _, preds = torch.max(outputs.logits, dim=1)
# 
#         predictions.extend(preds.cpu().tolist())
#         actual_labels.extend(labels.cpu().tolist())
# 
# print (classification_report (actual_labels, predictions))
# 
# model.save_pretrained("../models/model_copd")
# tokenizer.save_pretrained("../models/tokenizer_copd")

# import torch.nn.functional as F  # For softmax
# 
# # 2. Define a function to make predictions with scores
# def predict_with_scores(text):
#     encoding = tokenizer.encode_plus(
#         text,
#         add_special_tokens=True,
#         max_length=128,  # Or your trained max length
#         return_token_type_ids=False,
#         padding='max_length',
#         return_attention_mask=True,
#         return_tensors='pt',
#         truncation=True
#     )
# 
#     with torch.no_grad():
#         input_ids = encoding['input_ids'].to(device)
#         attention_mask = encoding['attention_mask'].to(device)
# 
#         outputs = model(input_ids, attention_mask=attention_mask)
#         logits = outputs.logits  # Get the raw logits
# 
#         # Apply softmax to get probabilities
#         probabilities = F.softmax(logits, dim=1)
# 
#         # Get the predicted class and its probability
#         predicted_class = torch.argmax(probabilities, dim=1).cpu().item()
#         class_probabilities = probabilities.detach().cpu().numpy().tolist()[0] # Detach before converting to numpy.
# 
#     return predicted_class, class_probabilities # Returns the predicted class and a list of probabilities.
# 
# 
# # 3. Example usage
# new_texts = [
#     "1. No significant interval change in moderate to advanced interstitial lung disease, indeterminate for UIP. Differential considerations include chronic hypersensitivity pneumonitis and atypical IPF. 2. Indeterminate pulmonary nodules measuring up to 3 mm. 3. Pectus excavatum. 4. Moderate coronary artery calcifications. 5. Small hiatal hernia with possible reflux related esophagitis.",
#     "1. Stable findings status post right lower lobectomy with postoperative changes as described below. No findings of residual or recurrent disease within the chest. 2. Mild to moderate peripheral interstitial fibrosis, without progression. These findings are favored to represent NSIP or UIP. Follow up recommendation for INCIDENTAL pulmonary nodule(s): Not applicable. [N] ATTENDING RADIOLOGIST AGREEMENT: I have personally reviewed the images and agree with this report.",
#     "The newly described nodular lung opacities have mostly resolved. Pulmonary nodule follow-up recommendation: [LN17N] Not Applicable.",
#     "1. Multistation enlarged thoracic lymph nodes which have overall increased in size in interval 2. Upper lung predominant pulmonary fibrosis Pulmonary nodule follow-up recommendation: [LN17N] Not Applicable.",
#     "1. Scattered stable bilateral lung nodules although many of the nodules have decreased or have resolved. Continued follow-up in one year recommended to assess for continued stability 2. Stable mildly enlarged multiple mediastinal lymph nodes, likely reactive. Indication: Nonspecific abnormal results from pulmonary function tests Comparison: May 15, 2014 FINDINGS: Lungs and Pleural Spaces: There are scattered tiny lung nodules in the lungs bilaterally as follow: 4 mm nodule right upper lobe image 110 series 6, stable. 4 mm nodule left lower lobe image 102, stable. 3 mm nodule left upper lobe more laterally image 100, stable. The other nodules seen on the prior exam have either resolved or much smaller. There is mild air trapping. There is probable mild centrilobular emphysema. There is bibasilar atelectasis. There are no pleural effusions. The airways are unremarkable. Thoracic Lymph Nodes: There are multiple borderline and mildly enlarged lymph nodes in the mediastinum but these are all stable since the prior exam. They are located in the prevascular area, subcarinal area, paratracheal area and AP window. Sample nodes include a 15 mm subcarinal lymph node on image 24 of series 2 and a 7 x 14 mm prevascular lymph node on image 12 series 2. There are scattered subcentimeter bilateral axillary lymph nodes. Cardiovascular, Mediastinal Structures and Thyroid: The heart size is top normal. There is stable mild dilatation of the left atrium. There is no pericardial effusion. There is mild coronary artery calcification. There is mild mitral annular calcification and aortic annular calcification. The aorta is not enlarged and is unremarkable. The pulmonary arteries are not enlarged. The thyroid is normal. The esophagus is within limits of normal. Skeleton, Diaphragm and Chest Wall: There are mild degenerative changes in the thoracic spine. The chest wall is unremarkable. Upper Abdomen: There is borderline splenomegaly at 13 cm. There is stable mild lobulation at the upper pole of left kidney, likely representing some cysts. There is a small splenule anterior and medial to the spleen. Technique: CT of the chest was performed from the lung apices to the lung bases, without intravenous administration of contrast. Images were obtained at inspiration and viewed in axial (including thin sections), sagittal and coronal planes."
# ]
# 
# for text in new_texts:
#     prediction, scores = predict_with_scores(text)
#     print(f"Text: {text}")
#     print(f"Prediction: {prediction}")
#     print(f"Scores: {scores}") # Probabilities corresponding to each class.
#     print("-" * 20)

# The code below is functional, though performance is not quite what would be expected

# from transformers import AutoTokenizer, AutoModelForSequenceClassification
# from torch.utils.data import DataLoader, Dataset
# import torch
# import pandas as pd
# from sklearn.model_selection import train_test_split
# from sklearn.metrics import classification_report
# 
# model_name = "emilyalsentzer/Bio_ClinicalBERT"
# tokenizer = AutoTokenizer.from_pretrained(model_name)
# model = AutoModelForSequenceClassification.from_pretrained(model_name, num_labels = 2) 
# 
# df = pd.read_csv("../data/ild_labeled.csv")
# 
# train_df, test_df = train_test_split (df, test_size = 0.2, random_state = 42)
# 
# class RadiologyDataset (Dataset):
#     def __init__(self, texts, labels, tokenizer, max_len):
#         self.texts = texts.tolist() if isinstance(texts, pd.Series) else texts
#         self.labels = labels.tolist() if isinstance(labels, pd.Series) else labels
#         self.tokenizer = tokenizer
#         self.max_len = max_len
# 
#     def __len__(self):
#         return len(self.texts)
# 
#     def __getitem__(self, item):
#         text = str(self.texts[item])
#         label = self.labels[item]
# 
#         encoding = self.tokenizer.encode_plus(
#             text,
#             add_special_tokens=True,
#             max_length=self.max_len,
#             return_token_type_ids=False,
#             padding='max_length',
#             return_attention_mask=True,
#             return_tensors='pt',
#             truncation=True
#         )
# 
#         return {
#             'input_ids': encoding['input_ids'].flatten(),
#             'attention_mask': encoding['attention_mask'].flatten(),
#             'labels': torch.tensor(label, dtype=torch.long)
#         }
# 
# MAX_LEN = 256
# BATCH_SIZE = 64
# 
# train_dataset = RadiologyDataset(train_df.text, train_df.label, tokenizer, MAX_LEN)
# test_dataset = RadiologyDataset(test_df.text, test_df.label, tokenizer, MAX_LEN)
# 
# train_dataloader = DataLoader(train_dataset, batch_size = BATCH_SIZE, shuffle = True)
# test_dataloader = DataLoader(test_dataset, batch_size = BATCH_SIZE)
# 
# device = torch.device('cuda' if torch.cuda.is_available() else 'cpu')
# model.to(device)
# 
# optimizer = torch.optim.AdamW (model.parameters (), lr = 2e-5)
# epochs = 10
# 
# for epoch in range(epochs):
#     model.train ()
#     total_loss = 0
#     for batch in train_dataloader:
#         input_ids = batch ['input_ids'].to(device)
#         attention_mask = batch ['attention_mask'].to(device)
#         labels = batch ['labels'].to(device)
# 
#         optimizer.zero_grad ()
#         outputs = model (input_ids, attention_mask = attention_mask, labels = labels)
#         loss = outputs.loss
#         total_loss += loss.item ()
#         loss.backward ()
#         optimizer.step ()
# 
#     print(f"Epoch {epoch+1}/{epochs}, Loss: {total_loss/len(train_dataloader)}")
# 
# model.eval ()
# predictions = []
# actual_labels = []
# 
# with torch.no_grad():
#     for batch in test_dataloader:
#         input_ids = batch['input_ids'].to(device)
#         attention_mask = batch['attention_mask'].to(device)
#         labels = batch['labels'].to(device)
# 
#         outputs = model(input_ids, attention_mask=attention_mask)
#         _, preds = torch.max(outputs.logits, dim=1)
# 
#         predictions.extend(preds.cpu().tolist())
#         actual_labels.extend(labels.cpu().tolist())
# 
# print (classification_report (actual_labels, predictions))

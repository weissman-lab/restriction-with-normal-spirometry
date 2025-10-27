# icd_ed_respiratory <- c (
#   
#   "D86.9", # Sarcoidosis, unspecified
#   "E84.9", # Cystic fibrosis, unspecified
#   "I27.20", # Pulmonary hypertension, unspecified
#   "J06.9", # Acute upper respiratory infection, unspecified
#   "J10.1", # Influenza due to other identified influenza virus with other respiratory manifestations
#   "J11.1", # Influenza due to unidentified influenza virus with other respiratory manifestations
#   "J18.9", # Pneumonia, unspecified organism
#   "J20.8", # Acute bronchitis due to other specified organisms
#   "J20.9", # Acute bronchitis, unspecified
#   "J40", # Bronchitis, not specified as acute or chronic
#   "J43.9", # Emphysema, unspecified
#   "J44.1", # Chronic obstructive pulmonary disease with acute exacerbation
#   "J44.89", # Other chronic obstructive pulmonary disease
#   "J44.9", # Chronic obstructive pulmonary disease, unspecified
#   "J45.41", # Moderate persistent asthma with (acute) exacerbation
#   "J45.901", # Unspecified asthma with acute exacerbation
#   "J45.909", # Unspecified asthma, uncomplicated
#   "J47.9", # Bronchiectasis, uncomplicated
#   "J84.10", # Pulmonary fibrosis, unspecified
#   "J84.9", # Interstitial pulmonary disease, unspecified
#   "J90", # Pleural effusion, not elsewhere classified
#   "J93.9", # Pneumothorax, unspecified
#   "J96.00", # - Acute respiratory failure, unspecified whether with hypoxia or hypercapnia
#   "J96.01", # Acute respiratory failure with hypoxia
#   "J96.20", # Acute and chronic respiratory failure, unspecified whether with hypoxia or hypercapnia
#   "J96.21", # Acute and chronic respiratory failure with hypoxia
#   "J98.01", # Acute bronchospasm
#   "J98.4", # Other disorders of lung
#   "J98.8", # Other specified respiratory disorders
#   "J96.90", # Respiratory failure, unspecified, unspecified whether with hypoxia or hypercapnia
#   "J96.91", # Respiratory failure, unspecified with hypoxia
#   "J96.92", # Respiratory failure, unspecified with hypercapnia
#   "R04.2", # Hemoptysis
#   "R05.3", # Chronic cough
#   "R05.8", # Other specified cough
#   "R05.9", # Cough, unspecified
#   "R06.00", # Dyspnea, unspecified
#   "R06.02", # Shortness of breath
#   "R06.03", # Acute respiratory distress
#   "R06.09", # Other forms of dyspnea
#   "R06.2", # Wheezing
#   "R06.89", # Other abnormalities of breathing
#   "R07.81", # Pleuritic chest pain
#   "R09.A2", # Hypoxemia
#   "R09.02", # Hypoxemia
#   "R09.1", # Pleurisy
#   "R09.89", # Other specified symptoms and signs involving the circulatory and respiratory systems
#   "R91.8", # Other nonspecific abnormal finding of lung field
#   "U07.1" # COVID-19
#   
# )

icd_asthma <- c (
  
  "J45", # 	 Asthma 
  "J45.2", #  	 Mild Intermittent Asthma 
  "J45.20", #  	 Mild Intermittent Asthma, Uncomplicated 
  "J45.21", #  	 Mild Intermittent Asthma with Exacerbation 
  "J45.22", #  	 Mild Intermittent Asthma with Status Asthmaticus 
  "J45.3", #  	 Mild Persistent Asthma 
  "J45.30", #  	 Mild Persistent Asthma, Uncomplicated 
  "J45.31", #  	 Mild Persistent Asthma with Acute Exacerbation 
  "J45.32", #  	 Mild Persistent Asthma with Status Asthmaticus 
  "J45.4", #  	 Moderate Persistent Asthma 
  "J45.40", #  	 Moderate Persistent Asthma, Uncomplicated 
  "J45.41", #  	 Moderate Persistent Asthma with Exacerbation 
  "J45.42", #  	 Moderate Persistent Asthma with Status Asthmaticus 
  "J45.5", #  	 Severe Persistent Asthma 
  "J45.50", #  	 Severe Persistent Asthma, Uncomplicated 
  "J45.51", #  	 Severe Persistent Asthma with Exacerbation 
  "J45.52", #  	 Severe Persistent Asthma with Status Asthmaticus 
  "J45.9", #  	 Other and Unspecified Asthma 
  "J45.90", #  	 Unspecified Asthma 
  "J45.901", #  	 Unspecified Asthma with Exacerbation 
  "J45.902", #  	 Unspecified Asthma with Status Asthmaticus 
  "J45.909", #  	 Unspecified Asthma, Uncomplicated 
  "J45.99", #  	 Other Asthma 
  "J45.990", #  	 Exercised Induced Bronchospasm 
  "J45.991", #  	 Cough Variant Asthma 
  "J45.998" #  	 Other Asthma 

)

icd_bronchiectasis <- c (
  
  "E84.0", # 	 Cystic Fibrosis with Pulmonary Manifestations   
  "J47", #  	 Bronchiectasis 
  "J47.0", #  	 Bronchiectasis with Acute Lower Respiratory Infection 
  "J47.1", #  	 Bronchiectasis with Exacerbation 
  "J47.9" #  	 Bronchiectasis, Uncomplicated 
  
)

icd_copd <- c (
  
  "J41", # 	 Simple and Mucopurulent Chronic Bronchitis 
  "J41.0", #  	 Simple Chronic Bronchitis 
  "J41.1", #  	 Mucopurulent Chronic Bronchitis 
  "J41.8", #  	 Mixed Simple and Mucopurulent Chronic Bronchitis  
  "J42", #  	 Unspecified Chronic Bronchitis 
  "J43", #  	 Emphysema 
  "J43.0", #  	 Unilateral Emphysema 
  "J43.1", #  	  Panlobular Emphysema 
  "J43.2", #  	 Centrilobular Emphysema 
  "J43.8", #  	 Other Emphysema 
  "J43.9", #  	 Emphysema, Unspecified 
  "J44", #  	 Other Chronic Obstructive Pulmonary Disease 
  "J44.0", #  	 Chronic Obstructive Pulmonary Disease with Lower Respiratory infection 
  "J44.1", #  	 Chronic Obstructive Pulmonary Disease with Exacerbation 
  "J44.8", #  	 Other Specified Chronic Obstructive Pulmonary Disease 
  "J44.89", #  	 Other Specified Chronic Obstructive Pulmonary Disease 
  "J44.9" #	" Chronic Obstructive Pulmonary Disease, Unspecified 	"  
  
)

icd_chest_wall <- c (
  
  "E66.2", # 	 Morbid Obesity with Alveolar Hypoventilation   
  "I27.1", # 	 Kyphoscoliotic heart disease
  "M40", # 	 Kyphosis and lordosis 
  "M40.0", # 	 Postural kyphosis 
  "M40.00", # 	 Postural kyphosis site unspecified 
  "M40.03", # 	 Postural kyphosis cervicothoracic region 
  "M40.04", # 	 Postural kyphosis thoracic region 
  "M40.05", # 	 Postural kyphosis thoracolumbar region 
  "M40.1", # 	 Other secondary kyphosis 
  "M40.10", # 	 Other secondary site unspecified 
  "M40.12", # 	 Other secondary cervical region 
  "M40.13", # 	 Other secondary cervicothoracic region 
  "M40.14", # 	 Other secondary thoracic region 
  "M40.15", # 	 Other secondary thoracolumbar region 
  "M40.2", # 	 Other and unspecified kyphosis 
  "M40.20", # 	 Unspecified kyphosis 
  "M40.202", # 	 Unspecified kyphosis cervical region 
  "M40.203", # 	 Unspecified kyphosis cervicothoracic region 
  "M40.204", # 	 Unspecified kyphosis thoracic region 
  "M40.205", # 	 Unspecified kyphosis thoracolumbar region 
  "M40.209", # 	 Unspecified kyphosis site unspecified 
  "M40.29", # 	 Other kyphosis 
  "M40.292", # 	 Other kyphosis cervical region 
  "M40.293", # 	 Other kyphosis cervicothoracic region 
  "M40.294", # 	 Other kyphosis thoracic region 
  "M40.295", # 	 Other kyphosis thoracolumbar region 
  "M40.299", # 	 Other kyphosis site unspecified 
  "M40.3", # 	 Flatback syndrome 
  "M40.30", # 	 Flatback syndrome site unspecified 
  "M40.35", # 	 Flatback syndrome thoracolumbar region 
  "M40.36", # 	 Flatback syndrome lumbar region 
  "M40.37", # 	 Flatback syndrome lumbosacral region 
  "M40.4", # 	 Postural lordosis 
  "M40.40", # 	 Postural lordosis site unspecified 
  "M40.45", # 	 Postural lordosis thoracolumbar region 
  "M40.46", # 	 Postural lordosis lumbar region 
  "M40.47", # 	 Postural lordosis lumbosacral region 
  "M40.5", # 	 Lordosis, unspecified 
  "M40.50", # 	 Lordosis site unspecified 
  "M40.55", # 	 Lordosis thoracolumbar region 
  "M40.56", # 	 Lordosis lumbar region 
  "M40.57", # 	 Lordosis lumbosacral region   
  "M41", # 	 Scoliosis 
  "M45", # 	 Ankylosing Spondylitis 
  "M45.0", # 	 Ankylosing spondylitis of multiple sites in spine 
  "M45.1", # 	 Ankylosing spondylitis of occipito-atlanto-axial region 
  "M45.2", # 	 Ankylosing spondylitis of cervical region 
  "M45.3", # 	 Ankylosing spondylitis of cervicothoracic region 
  "M45.4", # 	 Ankylosing spondylitis of thoracic region 
  "M45.5", # 	 Ankylosing spondylitis of thoracolumbar region 
  "M45.6", # 	 Ankylosing spondylitis lumbar region 
  "M45.7", #	 Ankylosing spondylitis of lumbosacral region 
  "M45.8", # 	 Ankylosing spondylitis sacral and sacrococcygeal region 
  "M45.9", # 	 Ankylosing spondylitis of unspecified sites in spine 
  "Q67.5", # 	 Congenital deformity of spine 
  "Q67.6", # 	 Pectus excavatum 
  "Q67.7", # 	 Pectus carinatum 
  "Q67.8" # 	 Other congenital deformities of chest   
)

icd_ild <- c (
  
  "D86.0", # 	 Sarcoidosis of the Lung 
  "J60", # 	 Coalworker's Pneumoconiosis 
  "J61", # 	 Pneumoconiosis Due to Asbestos and Other Mineral Fibers 
  "J62", # 	 Pneumoconiosis Due to Dust Containing Silica 
  "J62.0", # 	 Pneumoconiosis Due to Talc Dust 
  "J62.8", # 	 Pneumoconiosis Due to Other Dust Containing Silica 
  "J63", # 	 Pneumoconiosis Due to Other Inorganic Dusts 
  "J63.0", # 	 Aluminosis 
  "J63.1", # 	 Bauxite fibrosis 
  "J63.2", # 	 Berylliosis 
  "J63.3", # 	 Graphite Fibrosis 
  "J63.4", # 	 Siderosis 
  "J63.5", # 	 Stannosis 
  "J63.6", # 	 Pneumoconiosis Due to Other Specified Inorganic Dusts 
  "J64", # 	 Unspecified Pneumoconiosis 
  "J65", # 	 Pneumoconiosis Associated with Tuberculosis 
  "J67", # 	 Hypersensitivity Pneumonitis Due to Organic Dust 
  "J67.0", # 	 Farmer's Lung 
  "J67.1", # 	 Bagassosis 
  "J67.2", # 	 Bird Fancier's Lung 
  "J67.3", # 	 Suberosis 
  "J67.4", # 	 Maltworker's Lung 
  "J67.5", # 	 Mushroom-Worker's Lung 
  "J67.6", # 	 Maple-Bark-Stripper's Lung 
  "J67.7", # 	 Air Conditioner and Humidifier Lung 
  "J67.8", # 	 Hypersensitivity Pneumonitis Due to Other Organic Dusts 
  "J67.9", # 	 Hypersensitivity Pneumonitis to Unspecified Organic Dust 
  "J69", # 	 Pneumonitis Due to Solids and Liquids 
  "J69.0", # 	 Pneumonitis due to inhalation of food and vomit 
  "J69.1", # 	 Pneumonitis due to inhalation of oils and essences 
  "J69.8", # 	 Pneumonitis due to inhalation of other solids and liquids 
  "J70", # 	 Respiratory Conditions Due to Other External Agents 
  "J70.0", # 	 Acute Pulmonary Manifestations Due to Radiation 
  "J70.1", # 	 Chronic and Other Pulmonary Manifestations Due to Radiation 
  "J70.2", # 	 Acute Drug-Induced Interstitial Lung Disorders 
  "J70.3", # 	 Chronic Drug-Induced Interstitial Lung Disorders 
  "J70.4", # 	 Drug-Induced Interstitial Lung Disorders, Unspecified 
  "J82", # 	 Pulmonary Eosinophilia, Not Elsewhere Classified 
  "J82.81", # 	 Chronic Eosinophilic Pneumonia 
  "J82.82", # 	 Acute Eosinophilic Pneumonia 
  "J82.89", # 	 Other Pulmonary Eosinophilia, Not Elsewhere Classified 
  "J84", # 	 Other Interstitial Pulmonary Diseases 
  "J84.0", # 	 Alveolar and Parieto-Alveolar Conditions 
  "J84.01", # 	 Alveolar Proteinosis 
  "J84.02", # 	 Pulmonary Alveolar Microlithiasis 
  "J84.03", # 	 Idiopathic Pulmonary Hemosiderosis 
  "J84.09", # 	 Other Alveolar and Parieto-Alveolar Conditions 
  "J84.1", # 	 Other Interstitial Pulmonary Diseases with Fibrosis 
  "J84.10", # 	 Pulmonary Fibrosis, Unspecified 
  "J84.11", # 	 Idiopathic Interstitial Pneumonia 
  "J84.111", # 	 Idiopathic Interstitial Pneumonia, Not Otherwise Specified 
  "J84.112", # 	 Idiopathic Pulmonary Fibrosis 
  "J84.113", # 	 Idiopathic Non-Specific Interstitial Pneumonitis 
  "J84.114", # 	 Acute Interstitial Pneumonitis 
  "J84.115", # 	 Respiratory Bronchiolitis Interstitial Lung Disease 
  "J84.116", # 	 Cryptogenic Organizing Pneumonia 
  "J84.117", # 	 Desquamative Interstitial Pneumonia 
  "J84.17", # 	 Other Interstitial Pulmonary Diseases with Fibrosis in Diseases Classified Elsewhere 
  "J84.170", # 	 Interstitial Lung Disease with Progressive Fibrotic Phenotype in Diseases Classified Elsewhere 
  "J84.178", # 	 Other Interstitial Pulmonary Diseases with Fibrosis in Diseases Classified Elsewhere 
  "J84.2", # 	 Lymphoid Interstitial Pneumonia 
  "J84.8", # 	 Other Specified Interstitial Pulmonary Diseases 
  "J84.81", # 	 Lymphangioleiomyomatosis 
  "J84.82", # 	 Adult Pulmonary Langerhans Cell Histiocytosis 
  "J84.89", # 	 Other specified interstitial pulmonary disease 
  "J84.9", # 	 Interstitial Pulmonary Disease, Unspecified   
  "M05.01", # 	 Rheumatoid Lung Disease with Rheumatoid Arthritis of Unspecified Site 
  "M05.1", # 	 Rheumatoid Lung Disease with Rheumatoid Arthritis 
  "M05.10", # 	 Rheumatoid Lung Disease with Rheumatoid Arthritis of Unspecified Site 
  "M05.11", # 	 Rheumatoid lung disease with rheumatoid arthritis of shoulder 
  "M05.111", # 	 Rheumatoid lung disease with rheumatoid arthritis of right shoulder 
  "M05.112", # 	 Rheumatoid lung disease with rheumatoid arthritis of left shoulder 
  "M05.119", # 	 Rheumatoid lung disease with rheumatoid arthritis of unspecified shoulder 
  "M05.12", # 	 Rheumatoid lung disease with rheumatoid arthritis of elbow 
  "M05.121", # 	 Rheumatoid lung disease with rheumatoid arthritis of right elbow 
  "M05.122", # 	 Rheumatoid lung disease with rheumatoid arthritis of left elbow 
  "M05.129", # 	 Rheumatoid lung disease with rheumatoid arthritis of unspecified elbow 
  "M05.13", # 	 Rheumatoid lung disease with rheumatoid arthritis of wrist 
  "M05.131", # 	 Rheumatoid lung disease with rheumatoid arthritis of right wrist 
  "M05.132", # 	 Rheumatoid lung disease with rheumatoid arthritis of left wrist 
  "M05.139", # 	 Rheumatoid lung disease with rheumatoid arthritis of unspecified wrist 
  "M05.14", # 	 Rheumatoid lung disease with rheumatoid arthritis of hand 
  "M05.141", # 	 Rheumatoid lung disease with rheumatoid arthritis of right hand 
  "M05.142", # 	 Rheumatoid lung disease with rheumatoid arthritis of left hand 
  "M05.149", # 	 Rheumatoid lung disease with rheumatoid arthritis of unspecified hand 
  "M05.15", # 	 Rheumatoid lung disease with rheumatoid arthritis of hip 
  "M05.151", # 	 Rheumatoid lung disease with rheumatoid arthritis of right hip 
  "M05.152", # 	 Rheumatoid lung disease with rheumatoid arthritis of left hip 
  "M05.159", # 	 Rheumatoid lung disease with rheumatoid arthritis of unspecified hip 
  "M05.16", # 	 Rheumatoid lung disease with rheumatoid arthritis of knee 
  "M05.161", # 	 Rheumatoid lung disease with rheumatoid arthritis of right knee 
  "M05.162", # 	 Rheumatoid lung disease with rheumatoid arthritis of left knee 
  "M05.169", # 	 Rheumatoid lung disease with rheumatoid arthritis of unspecified knee 
  "M05.17", # 	 Rheumatoid lung disease with rheumatoid arthritis of ankle and foot 
  "M05.171", # 	 Rheumatoid lung disease with rheumatoid arthritis of right ankle and foot 
  "M05.172", # 	 Rheumatoid lung disease with rheumatoid arthritis of left ankle and foot 
  "M05.179", # 	 Rheumatoid lung disease with rheumatoid arthritis of unspecified ankle and foot 
  "M05.19", # 	 Rheumatoid Lung Disease with Rheumatoid Arthritis of Multiple Sites 
  "M05.19", # 	 Rheumatoid Lung Disease with Rheumatoid Arthritis of Multiple Sites 
  "M32.13", # 	 Lung involvement in systemic lupus erythematosus
  "M33.01", # 	 Juvenile dermatomyositis with respiratory involvement
  "M33.11", # 	 Other Dermatopolymyositis with Lung Involvement 
  "M33.21", # 	 Polymyositis with Respiratory Involvement 
  "M33.91", # 	 Dermatopolymyositis with Respiratory Involvement 
  "M34.81", #	 Systemic sclerosis with lung involvement
  "M35.02" #	 Sjögren syndrome with lung involvement
  
)

icd_neuromuscular <- c (
  
  "G12", # 	 Spinal muscular atrophy and related syndromes 
  "G12.0", # 	 Infantile spinal muscular atrophy, type I [Werdnig-Hoffman] 
  "G12.1", # 	 Other inherited spinal muscular atrophy 
  "G12.2", # 	 Motor neuron disease 
  "G12.20", # 	 Motor neuron disease unspecified 
  "G12.21", # 	 Amyotrophic lateral sclerosis 
  "G12.22", # 	 Progressive bulbar palsy 
  "G12.23", # 	 Primary lateral sclerosis 
  "G12.24", # 	 Familial motor neuron disease 
  "G12.25", # 	 Progressive spinal muscle atrophy 
  "G12.29", # 	 Other motor neuron disease 
  "G12.8", # 	 Other spinal muscular atrophies and related syndromes 
  "G12.9", # 	 Spinal muscular atrophy, unspecified 
  "G13.1", #	 Other systemic atrophy primarily affecting central nervous system in neoplastic disease
  "G35", #	 Multiple sclerosis
  "G61", #	 Inflammatory polyneuropathy
  "G61.0", #	 Guillain-Barre syndrome
  "G61.8", #	 Other inflammatory polyneuropathies
  "G61.81", #	 Chronic inflammatory demyelinating polyneuritis
  "G61.82", #	 Multifocal motor neuropathy
  "G61.89", #	 Other inflammatory polyneuropathies
  "G61.9", #	 Inflammatory polyneuropathy, unspecified
  "G65", #	 Sequelae of inflammatory and toxic polyneuropathies
  "G65.0", #	 Sequelae of Guillain-Barré syndrome
  "G65.1", #	 Sequelae of other inflammatory polyneuropathy
  "G65.2", #	 Sequelae of toxic polyneuropathy
  "G70", # 	 Myasthenia gravis and other myoneural disorders 
  "G70.0", # 	 Myasthenia gravis 
  "G70.00", # 	 Myasthenia gravis without (acute) exacerbation 
  "G70.01", # 	 Myasthenia gravis with (acute) exacerbation 
  "G70.1", # 	 Toxic myoneural disorders 
  "G70.2", # 	 Congenital and developmental myasthenia 
  "G70.8", # 	 Other specified myoneural disorders 
  "G70.80", # 	 Lambert-Eaton syndrome, unspecified 
  "G70.81", # 	 Lambert-Eaton syndrome in disease classified elsewhere 
  "G70.89", # 	 Other specified myoneural disorders 
  "G70.9", # 	 Myoneural disorder, unspecified 
  "G71", #	 Primary disorders of muscles
  "G71.0", #	 Muscular dystrophy
  "G71.00", #	 Muscular dystrophy unspecified
  "G71.01", #	 Duchenne or Becker muscular dystrophy
  "G71.02", #	 Facioscapulohumeral muscular dystrophy
  "G71.03", #	 Limb girdle muscular dystrophies
  "G71.031", #	 Autosomal dominant limb girdle muscular dystrophy
  "G71.032", #	 Autosomal recessive limb girdle muscular dystrophy due to calpain-3 dysfunction
  "G71.033", #	 Limb girdle muscular dystrophy due to dysferlin dysfunction
  "G71.034", #	 Limb girdle muscular dystrophy due to sarcoglycan dysfunction
  "G71.0340", #	 Limb girdle muscular dystrophy due to sarcoglycan dysfunction unspecified
  "G71.0341", #	 Limb girdle muscular dystrophy due to alpha sarcoglycan dysfunction
  "G71.0342", #	 Limb girdle muscular dystrophy due to beta sarcoglycan dysfunction
  "G71.0349", #	 Limb girdle muscular dystrophy due to other sarcoglycan dysfunction
  "G71.035", #	 Limb girdle muscular dystrophy due to anoctamin-5 dysfunction
  "G71.038", #	 Other limb girdle muscular dystrophy
  "G71.039", #	 Limb girdle muscular dystrophy, unspecified
  "G71.09", #	 Other specified muscular dystrophies
  "G71.1", #	 Myotonic disorders
  "G71.11", #	 Myotonic muscular dystrophy
  "G71.12", #	 Myotonia congenita
  "G71.13", #	 Myotonic chondrodystrophy
  "G71.14", #	 Drug induced myotonia
  "G71.19", #	 Other specified myotonic disorders
  "G71.2", #	 Congenital myopathies
  "G71.20", #	 Congenital myopathy, unspecified
  "G71.21", #	 Nemaline myopathy
  "G71.22", #	 Centronuclear myopathy
  "G71.220", #	 X-linked myotubular myopathy
  "G71.228", #	 Other centronuclear myopathy
  "G71.29", #	 Other congenital myopathy
  "G71.3", #	 Mitochondrial myopathy, not elsewhere classified
  "G71.8", #	 Other primary disorders of muscles
  "G71.9", #	 Primary disorder of muscle, unspecified
  "G72.4", #	 Inflammatory and immune myopathies, not elsewhere classified
  "G72.41", #	 Inclusion body myositis
  "G72.49", #	 Other inflammatory and immune myopathies, not elsewhere classified
  "G73", #	 Disorders of myoneural junction and muscle in diseases classified elsewhere
  "G73.1", #	 Lambert-Eaton syndrome in neoplastic disease
  "G73.3", #	 Myasthenic syndromes in other diseases classified elsewhere
  "G73.7" #	 Myopathy in diseases classified elsewhere
  
)

icd_ph <- c (
  
  "I27", # 	 Other pulmonary heart diseases 
  "I27.0", # 	 Primary pulmonary hypertension 
  "I27.1", # 	 Kyphoscoliotic heart disease 
  "I27.2", # 	 Other secondary pulmonary hypertension 
  "I27.20", # 	 Pulmonary hypertension, unspecified 
  "I27.21", # 	 Secondary pulmonary arterial hypertension 
  "I27.22", # 	 Pulmonary hypertension due to left heart disease 
  "I27.23", # 	 Pulmonary hypertension due to lung diseases and hypoxia 
  "I27.24", # 	 Chronic thromboembolic pulmonary hypertension 
  "I27.29", # 	 Other secondary pulmonary hypertension 
  "I27.8", # 	 Other specified pulmonary heart diseases 
  "I27.81", # 	 Cor pulmonale (chronic) 
  "I27.82", # 	 Chronic pulmonary embolism 
  "I27.83", # 	 Eisenmenger's syndrome 
  "I27.89", # 	 Other specified pulmonary heart diseases 
  "I27.9" # 	 Pulmonary heart disease, unspecified   
  
)

# icd_rheumatic <- c (
#   
#   "M04", # Autoinflammatory syndromes
#   "M04.1", #  Periodic fever syndromes
#   "M04.2", #  Cryopyrin-associated periodic syndromes
#   "M04.8", #  Other autoinflammatory syndromes
#   "M04.9", #  Autoinflammatory syndrome, unspecified
#   "M05", #  Rheumatoid arthritis with rheumatoid factor
#   "M05.0", #  Felty's syndrome
#   "M05.00", #  …… unspecified site
#   "M05.01", #  Felty's syndrome, shoulder
#   "M05.011", #  Felty's syndrome, right shoulder
#   "M05.012", #  Felty's syndrome, left shoulder
#   "M05.019", #  Felty's syndrome, unspecified shoulder
#   "M05.02", #  Felty's syndrome, elbow
#   "M05.021", #  Felty's syndrome, right elbow
#   "M05.022", #  Felty's syndrome, left elbow
#   "M05.029", #  Felty's syndrome, unspecified elbow
#   "M05.03", #  Felty's syndrome, wrist
#   "M05.031", #  Felty's syndrome, right wrist
#   "M05.032", #  Felty's syndrome, left wrist
#   "M05.039", #  Felty's syndrome, unspecified wrist
#   "M05.04", #  Felty's syndrome, hand
#   "M05.041", #  Felty's syndrome, right hand
#   "M05.042", #  Felty's syndrome, left hand
#   "M05.049", #  Felty's syndrome, unspecified hand
#   "M05.05", #  Felty's syndrome, hip
#   "M05.051", #  Felty's syndrome, right hip
#   "M05.052", #  Felty's syndrome, left hip
#   "M05.059", #  Felty's syndrome, unspecified hip
#   "M05.06", #  Felty's syndrome, knee
#   "M05.061", #  Felty's syndrome, right knee
#   "M05.062", #  Felty's syndrome, left knee
#   "M05.069", #  Felty's syndrome, unspecified knee
#   "M05.07", #  Felty's syndrome, ankle and foot
#   "M05.071", #  Felty's syndrome, right ankle and foot
#   "M05.072", #  Felty's syndrome, left ankle and foot
#   "M05.079", #  Felty's syndrome, unspecified ankle and foot
#   "M05.09", #  …… multiple sites
#   "M05.1", #  Rheumatoid lung disease with rheumatoid arthritis
#   "M05.10", #  …… of unspecified site
#   "M05.11", #  Rheumatoid lung disease with rheumatoid arthritis of shoulder
#   "M05.111", #  Rheumatoid lung disease with rheumatoid arthritis of right shoulder
#   "M05.112", #  Rheumatoid lung disease with rheumatoid arthritis of left shoulder
#   "M05.119", #  Rheumatoid lung disease with rheumatoid arthritis of unspecified shoulder
#   "M05.12", #  Rheumatoid lung disease with rheumatoid arthritis of elbow
#   "M05.121", #  Rheumatoid lung disease with rheumatoid arthritis of right elbow
#   "M05.122", #  Rheumatoid lung disease with rheumatoid arthritis of left elbow
#   "M05.129", #  Rheumatoid lung disease with rheumatoid arthritis of unspecified elbow
#   "M05.13", #  Rheumatoid lung disease with rheumatoid arthritis of wrist
#   "M05.131", #  Rheumatoid lung disease with rheumatoid arthritis of right wrist
#   "M05.132", #  Rheumatoid lung disease with rheumatoid arthritis of left wrist
#   "M05.139", #  Rheumatoid lung disease with rheumatoid arthritis of unspecified wrist
#   "M05.14", #  Rheumatoid lung disease with rheumatoid arthritis of hand
#   "M05.141", #  Rheumatoid lung disease with rheumatoid arthritis of right hand
#   "M05.142", #  Rheumatoid lung disease with rheumatoid arthritis of left hand
#   "M05.149", #  Rheumatoid lung disease with rheumatoid arthritis of unspecified hand
#   "M05.15", #  Rheumatoid lung disease with rheumatoid arthritis of hip
#   "M05.151", #  Rheumatoid lung disease with rheumatoid arthritis of right hip
#   "M05.152", #  Rheumatoid lung disease with rheumatoid arthritis of left hip
#   "M05.159", #  Rheumatoid lung disease with rheumatoid arthritis of unspecified hip
#   "M05.16", #  Rheumatoid lung disease with rheumatoid arthritis of knee
#   "M05.161", #  Rheumatoid lung disease with rheumatoid arthritis of right knee
#   "M05.162", #  Rheumatoid lung disease with rheumatoid arthritis of left knee
#   "M05.169", #  Rheumatoid lung disease with rheumatoid arthritis of unspecified knee
#   "M05.17", #  Rheumatoid lung disease with rheumatoid arthritis of ankle and foot
#   "M05.171", #  Rheumatoid lung disease with rheumatoid arthritis of right ankle and foot
#   "M05.172", #  Rheumatoid lung disease with rheumatoid arthritis of left ankle and foot
#   "M05.179", #  Rheumatoid lung disease with rheumatoid arthritis of unspecified ankle and foot
#   "M05.19", #  …… of multiple sites
#   "M05.2", #  Rheumatoid vasculitis with rheumatoid arthritis
#   "M05.20", #  …… of unspecified site
#   "M05.21", #  Rheumatoid vasculitis with rheumatoid arthritis of shoulder
#   "M05.211", #  Rheumatoid vasculitis with rheumatoid arthritis of right shoulder
#   "M05.212", #  Rheumatoid vasculitis with rheumatoid arthritis of left shoulder
#   "M05.219", #  Rheumatoid vasculitis with rheumatoid arthritis of unspecified shoulder
#   "M05.22", #  Rheumatoid vasculitis with rheumatoid arthritis of elbow
#   "M05.221", #  Rheumatoid vasculitis with rheumatoid arthritis of right elbow
#   "M05.222", #  Rheumatoid vasculitis with rheumatoid arthritis of left elbow
#   "M05.229", #  Rheumatoid vasculitis with rheumatoid arthritis of unspecified elbow
#   "M05.23", #  Rheumatoid vasculitis with rheumatoid arthritis of wrist
#   "M05.231", #  Rheumatoid vasculitis with rheumatoid arthritis of right wrist
#   "M05.232", #  Rheumatoid vasculitis with rheumatoid arthritis of left wrist
#   "M05.239", #  Rheumatoid vasculitis with rheumatoid arthritis of unspecified wrist
#   "M05.24", #  Rheumatoid vasculitis with rheumatoid arthritis of hand
#   "M05.241", #  Rheumatoid vasculitis with rheumatoid arthritis of right hand
#   "M05.242", #  Rheumatoid vasculitis with rheumatoid arthritis of left hand
#   "M05.249", #  Rheumatoid vasculitis with rheumatoid arthritis of unspecified hand
#   "M05.25", #  Rheumatoid vasculitis with rheumatoid arthritis of hip
#   "M05.251", #  Rheumatoid vasculitis with rheumatoid arthritis of right hip
#   "M05.252", #  Rheumatoid vasculitis with rheumatoid arthritis of left hip
#   "M05.259", #  Rheumatoid vasculitis with rheumatoid arthritis of unspecified hip
#   "M05.26", #  Rheumatoid vasculitis with rheumatoid arthritis of knee
#   "M05.261", #  Rheumatoid vasculitis with rheumatoid arthritis of right knee
#   "M05.262", #  Rheumatoid vasculitis with rheumatoid arthritis of left knee
#   "M05.269", #  Rheumatoid vasculitis with rheumatoid arthritis of unspecified knee
#   "M05.27", #  Rheumatoid vasculitis with rheumatoid arthritis of ankle and foot
#   "M05.271", #  Rheumatoid vasculitis with rheumatoid arthritis of right ankle and foot
#   "M05.272", #  Rheumatoid vasculitis with rheumatoid arthritis of left ankle and foot
#   "M05.279", #  Rheumatoid vasculitis with rheumatoid arthritis of unspecified ankle and foot
#   "M05.29", #  …… of multiple sites
#   "M05.3", #  Rheumatoid heart disease with rheumatoid arthritis
#   "M05.30", #  …… of unspecified site
#   "M05.31", #  Rheumatoid heart disease with rheumatoid arthritis of shoulder
#   "M05.311", #  Rheumatoid heart disease with rheumatoid arthritis of right shoulder
#   "M05.312", #  Rheumatoid heart disease with rheumatoid arthritis of left shoulder
#   "M05.319", #  Rheumatoid heart disease with rheumatoid arthritis of unspecified shoulder
#   "M05.32", #  Rheumatoid heart disease with rheumatoid arthritis of elbow
#   "M05.321", #  Rheumatoid heart disease with rheumatoid arthritis of right elbow
#   "M05.322", #  Rheumatoid heart disease with rheumatoid arthritis of left elbow
#   "M05.329", #  Rheumatoid heart disease with rheumatoid arthritis of unspecified elbow
#   "M05.33", #  Rheumatoid heart disease with rheumatoid arthritis of wrist
#   "M05.331", #  Rheumatoid heart disease with rheumatoid arthritis of right wrist
#   "M05.332", #  Rheumatoid heart disease with rheumatoid arthritis of left wrist
#   "M05.339", #  Rheumatoid heart disease with rheumatoid arthritis of unspecified wrist
#   "M05.34", #  Rheumatoid heart disease with rheumatoid arthritis of hand
#   "M05.341", #  Rheumatoid heart disease with rheumatoid arthritis of right hand
#   "M05.342", #  Rheumatoid heart disease with rheumatoid arthritis of left hand
#   "M05.349", #  Rheumatoid heart disease with rheumatoid arthritis of unspecified hand
#   "M05.35", #  Rheumatoid heart disease with rheumatoid arthritis of hip
#   "M05.351", #  Rheumatoid heart disease with rheumatoid arthritis of right hip
#   "M05.352", #  Rheumatoid heart disease with rheumatoid arthritis of left hip
#   "M05.359", #  Rheumatoid heart disease with rheumatoid arthritis of unspecified hip
#   "M05.36", #  Rheumatoid heart disease with rheumatoid arthritis of knee
#   "M05.361", #  Rheumatoid heart disease with rheumatoid arthritis of right knee
#   "M05.362", #  Rheumatoid heart disease with rheumatoid arthritis of left knee
#   "M05.369", #  Rheumatoid heart disease with rheumatoid arthritis of unspecified knee
#   "M05.37", #  Rheumatoid heart disease with rheumatoid arthritis of ankle and foot
#   "M05.371", #  Rheumatoid heart disease with rheumatoid arthritis of right ankle and foot
#   "M05.372", #  Rheumatoid heart disease with rheumatoid arthritis of left ankle and foot
#   "M05.379", #  Rheumatoid heart disease with rheumatoid arthritis of unspecified ankle and foot
#   "M05.39", #  …… of multiple sites
#   "M05.4", #  Rheumatoid myopathy with rheumatoid arthritis
#   "M05.40", #  …… of unspecified site
#   "M05.41", #  Rheumatoid myopathy with rheumatoid arthritis of shoulder
#   "M05.411", #  Rheumatoid myopathy with rheumatoid arthritis of right shoulder
#   "M05.412", #  Rheumatoid myopathy with rheumatoid arthritis of left shoulder
#   "M05.419", #  Rheumatoid myopathy with rheumatoid arthritis of unspecified shoulder
#   "M05.42", #  Rheumatoid myopathy with rheumatoid arthritis of elbow
#   "M05.421", #  Rheumatoid myopathy with rheumatoid arthritis of right elbow
#   "M05.422", #  Rheumatoid myopathy with rheumatoid arthritis of left elbow
#   "M05.429", #  Rheumatoid myopathy with rheumatoid arthritis of unspecified elbow
#   "M05.43", #  Rheumatoid myopathy with rheumatoid arthritis of wrist
#   "M05.431", #  Rheumatoid myopathy with rheumatoid arthritis of right wrist
#   "M05.432", #  Rheumatoid myopathy with rheumatoid arthritis of left wrist
#   "M05.439", #  Rheumatoid myopathy with rheumatoid arthritis of unspecified wrist
#   "M05.44", #  Rheumatoid myopathy with rheumatoid arthritis of hand
#   "M05.441", #  Rheumatoid myopathy with rheumatoid arthritis of right hand
#   "M05.442", #  Rheumatoid myopathy with rheumatoid arthritis of left hand
#   "M05.449", #  Rheumatoid myopathy with rheumatoid arthritis of unspecified hand
#   "M05.45", #  Rheumatoid myopathy with rheumatoid arthritis of hip
#   "M05.451", #  Rheumatoid myopathy with rheumatoid arthritis of right hip
#   "M05.452", #  Rheumatoid myopathy with rheumatoid arthritis of left hip
#   "M05.459", #  Rheumatoid myopathy with rheumatoid arthritis of unspecified hip
#   "M05.46", #  Rheumatoid myopathy with rheumatoid arthritis of knee
#   "M05.461", #  Rheumatoid myopathy with rheumatoid arthritis of right knee
#   "M05.462", #  Rheumatoid myopathy with rheumatoid arthritis of left knee
#   "M05.469", #  Rheumatoid myopathy with rheumatoid arthritis of unspecified knee
#   "M05.47", #  Rheumatoid myopathy with rheumatoid arthritis of ankle and foot
#   "M05.471", #  Rheumatoid myopathy with rheumatoid arthritis of right ankle and foot
#   "M05.472", #  Rheumatoid myopathy with rheumatoid arthritis of left ankle and foot
#   "M05.479", #  Rheumatoid myopathy with rheumatoid arthritis of unspecified ankle and foot
#   "M05.49", #  …… of multiple sites
#   "M05.5", #  Rheumatoid polyneuropathy with rheumatoid arthritis
#   "M05.50", #  …… of unspecified site
#   "M05.51", #  Rheumatoid polyneuropathy with rheumatoid arthritis of shoulder
#   "M05.511", #  Rheumatoid polyneuropathy with rheumatoid arthritis of right shoulder
#   "M05.512", #  Rheumatoid polyneuropathy with rheumatoid arthritis of left shoulder
#   "M05.519", #  Rheumatoid polyneuropathy with rheumatoid arthritis of unspecified shoulder
#   "M05.52", #  Rheumatoid polyneuropathy with rheumatoid arthritis of elbow
#   "M05.521", #  Rheumatoid polyneuropathy with rheumatoid arthritis of right elbow
#   "M05.522", #  Rheumatoid polyneuropathy with rheumatoid arthritis of left elbow
#   "M05.529", #  Rheumatoid polyneuropathy with rheumatoid arthritis of unspecified elbow
#   "M05.53", #  Rheumatoid polyneuropathy with rheumatoid arthritis of wrist
#   "M05.531", #  Rheumatoid polyneuropathy with rheumatoid arthritis of right wrist
#   "M05.532", #  Rheumatoid polyneuropathy with rheumatoid arthritis of left wrist
#   "M05.539", #  Rheumatoid polyneuropathy with rheumatoid arthritis of unspecified wrist
#   "M05.54", #  Rheumatoid polyneuropathy with rheumatoid arthritis of hand
#   "M05.541", #  Rheumatoid polyneuropathy with rheumatoid arthritis of right hand
#   "M05.542", #  Rheumatoid polyneuropathy with rheumatoid arthritis of left hand
#   "M05.549", #  Rheumatoid polyneuropathy with rheumatoid arthritis of unspecified hand
#   "M05.55", #  Rheumatoid polyneuropathy with rheumatoid arthritis of hip
#   "M05.551", #  Rheumatoid polyneuropathy with rheumatoid arthritis of right hip
#   "M05.552", #  Rheumatoid polyneuropathy with rheumatoid arthritis of left hip
#   "M05.559", #  Rheumatoid polyneuropathy with rheumatoid arthritis of unspecified hip
#   "M05.56", #  Rheumatoid polyneuropathy with rheumatoid arthritis of knee
#   "M05.561", #  Rheumatoid polyneuropathy with rheumatoid arthritis of right knee
#   "M05.562", #  Rheumatoid polyneuropathy with rheumatoid arthritis of left knee
#   "M05.569", #  Rheumatoid polyneuropathy with rheumatoid arthritis of unspecified knee
#   "M05.57", #  Rheumatoid polyneuropathy with rheumatoid arthritis of ankle and foot
#   "M05.571", #  Rheumatoid polyneuropathy with rheumatoid arthritis of right ankle and foot
#   "M05.572", #  Rheumatoid polyneuropathy with rheumatoid arthritis of left ankle and foot
#   "M05.579", #  Rheumatoid polyneuropathy with rheumatoid arthritis of unspecified ankle and foot
#   "M05.59", #  …… of multiple sites
#   "M05.6", #  Rheumatoid arthritis with involvement of other organs and systems
#   "M05.60", #  Rheumatoid arthritis of unspecified site with involvement of other organs and systems
#   "M05.61", #  Rheumatoid arthritis of shoulder with involvement of other organs and systems
#   "M05.611", #  Rheumatoid arthritis of right shoulder with involvement of other organs and systems
#   "M05.612", #  Rheumatoid arthritis of left shoulder with involvement of other organs and systems
#   "M05.619", #  Rheumatoid arthritis of unspecified shoulder with involvement of other organs and systems
#   "M05.62", #  Rheumatoid arthritis of elbow with involvement of other organs and systems
#   "M05.621", #  Rheumatoid arthritis of right elbow with involvement of other organs and systems
#   "M05.622", #  Rheumatoid arthritis of left elbow with involvement of other organs and systems
#   "M05.629", #  Rheumatoid arthritis of unspecified elbow with involvement of other organs and systems
#   "M05.63", #  Rheumatoid arthritis of wrist with involvement of other organs and systems
#   "M05.631", #  Rheumatoid arthritis of right wrist with involvement of other organs and systems
#   "M05.632", #  Rheumatoid arthritis of left wrist with involvement of other organs and systems
#   "M05.639", #  Rheumatoid arthritis of unspecified wrist with involvement of other organs and systems
#   "M05.64", #  Rheumatoid arthritis of hand with involvement of other organs and systems
#   "M05.641", #  Rheumatoid arthritis of right hand with involvement of other organs and systems
#   "M05.642", #  Rheumatoid arthritis of left hand with involvement of other organs and systems
#   "M05.649", #  Rheumatoid arthritis of unspecified hand with involvement of other organs and systems
#   "M05.65", #  Rheumatoid arthritis of hip with involvement of other organs and systems
#   "M05.651", #  Rheumatoid arthritis of right hip with involvement of other organs and systems
#   "M05.652", #  Rheumatoid arthritis of left hip with involvement of other organs and systems
#   "M05.659", #  Rheumatoid arthritis of unspecified hip with involvement of other organs and systems
#   "M05.66", #  Rheumatoid arthritis of knee with involvement of other organs and systems
#   "M05.661", #  Rheumatoid arthritis of right knee with involvement of other organs and systems
#   "M05.662", #  Rheumatoid arthritis of left knee with involvement of other organs and systems
#   "M05.669", #  Rheumatoid arthritis of unspecified knee with involvement of other organs and systems
#   "M05.67", #  Rheumatoid arthritis of ankle and foot with involvement of other organs and systems
#   "M05.671", #  Rheumatoid arthritis of right ankle and foot with involvement of other organs and systems
#   "M05.672", #  Rheumatoid arthritis of left ankle and foot with involvement of other organs and systems
#   "M05.679", #  Rheumatoid arthritis of unspecified ankle and foot with involvement of other organs and systems
#   "M05.69", #  Rheumatoid arthritis of multiple sites with involvement of other organs and systems
#   "M05.7", #  Rheumatoid arthritis with rheumatoid factor without organ or systems involvement
#   "M05.70", #  Rheumatoid arthritis with rheumatoid factor of unspecified site without organ or systems involvement
#   "M05.71", #  Rheumatoid arthritis with rheumatoid factor of shoulder without organ or systems involvement
#   "M05.711", #  Rheumatoid arthritis with rheumatoid factor of right shoulder without organ or systems involvement
#   "M05.712", #  Rheumatoid arthritis with rheumatoid factor of left shoulder without organ or systems involvement
#   "M05.719", #  Rheumatoid arthritis with rheumatoid factor of unspecified shoulder without organ or systems involvement
#   "M05.72", #  Rheumatoid arthritis with rheumatoid factor of elbow without organ or systems involvement
#   "M05.721", #  Rheumatoid arthritis with rheumatoid factor of right elbow without organ or systems involvement
#   "M05.722", #  Rheumatoid arthritis with rheumatoid factor of left elbow without organ or systems involvement
#   "M05.729", #  Rheumatoid arthritis with rheumatoid factor of unspecified elbow without organ or systems involvement
#   "M05.73", #  Rheumatoid arthritis with rheumatoid factor of wrist without organ or systems involvement
#   "M05.731", #  Rheumatoid arthritis with rheumatoid factor of right wrist without organ or systems involvement
#   "M05.732", #  Rheumatoid arthritis with rheumatoid factor of left wrist without organ or systems involvement
#   "M05.739", #  Rheumatoid arthritis with rheumatoid factor of unspecified wrist without organ or systems involvement
#   "M05.74", #  Rheumatoid arthritis with rheumatoid factor of hand without organ or systems involvement
#   "M05.741", #  Rheumatoid arthritis with rheumatoid factor of right hand without organ or systems involvement
#   "M05.742", #  Rheumatoid arthritis with rheumatoid factor of left hand without organ or systems involvement
#   "M05.749", #  Rheumatoid arthritis with rheumatoid factor of unspecified hand without organ or systems involvement
#   "M05.75", #  Rheumatoid arthritis with rheumatoid factor of hip without organ or systems involvement
#   "M05.751", #  Rheumatoid arthritis with rheumatoid factor of right hip without organ or systems involvement
#   "M05.752", #  Rheumatoid arthritis with rheumatoid factor of left hip without organ or systems involvement
#   "M05.759", #  Rheumatoid arthritis with rheumatoid factor of unspecified hip without organ or systems involvement
#   "M05.76", #  Rheumatoid arthritis with rheumatoid factor of knee without organ or systems involvement
#   "M05.761", #  Rheumatoid arthritis with rheumatoid factor of right knee without organ or systems involvement
#   "M05.762", #  Rheumatoid arthritis with rheumatoid factor of left knee without organ or systems involvement
#   "M05.769", #  Rheumatoid arthritis with rheumatoid factor of unspecified knee without organ or systems involvement
#   "M05.77", #  Rheumatoid arthritis with rheumatoid factor of ankle and foot without organ or systems involvement
#   "M05.771", #  Rheumatoid arthritis with rheumatoid factor of right ankle and foot without organ or systems involvement
#   "M05.772", #  Rheumatoid arthritis with rheumatoid factor of left ankle and foot without organ or systems involvement
#   "M05.779", #  Rheumatoid arthritis with rheumatoid factor of unspecified ankle and foot without organ or systems involvement
#   "M05.79", #  Rheumatoid arthritis with rheumatoid factor of multiple sites without organ or systems involvement
#   "M05.7A", #  Rheumatoid arthritis with rheumatoid factor of other specified site without organ or systems involvement
#   "M05.8", #  Other rheumatoid arthritis with rheumatoid factor
#   "M05.80", #  …… of unspecified site
#   "M05.81", #  Other rheumatoid arthritis with rheumatoid factor of shoulder
#   "M05.811", #  Other rheumatoid arthritis with rheumatoid factor of right shoulder
#   "M05.812", #  Other rheumatoid arthritis with rheumatoid factor of left shoulder
#   "M05.819", #  Other rheumatoid arthritis with rheumatoid factor of unspecified shoulder
#   "M05.82", #  Other rheumatoid arthritis with rheumatoid factor of elbow
#   "M05.821", #  Other rheumatoid arthritis with rheumatoid factor of right elbow
#   "M05.822", #  Other rheumatoid arthritis with rheumatoid factor of left elbow
#   "M05.829", #  Other rheumatoid arthritis with rheumatoid factor of unspecified elbow
#   "M05.83", #  Other rheumatoid arthritis with rheumatoid factor of wrist
#   "M05.831", #  Other rheumatoid arthritis with rheumatoid factor of right wrist
#   "M05.832", #  Other rheumatoid arthritis with rheumatoid factor of left wrist
#   "M05.839", #  Other rheumatoid arthritis with rheumatoid factor of unspecified wrist
#   "M05.84", #  Other rheumatoid arthritis with rheumatoid factor of hand
#   "M05.841", #  Other rheumatoid arthritis with rheumatoid factor of right hand
#   "M05.842", #  Other rheumatoid arthritis with rheumatoid factor of left hand
#   "M05.849", #  Other rheumatoid arthritis with rheumatoid factor of unspecified hand
#   "M05.85", #  Other rheumatoid arthritis with rheumatoid factor of hip
#   "M05.851", #  Other rheumatoid arthritis with rheumatoid factor of right hip
#   "M05.852", #  Other rheumatoid arthritis with rheumatoid factor of left hip
#   "M05.859", #  Other rheumatoid arthritis with rheumatoid factor of unspecified hip
#   "M05.86", #  Other rheumatoid arthritis with rheumatoid factor of knee
#   "M05.861", #  Other rheumatoid arthritis with rheumatoid factor of right knee
#   "M05.862", #  Other rheumatoid arthritis with rheumatoid factor of left knee
#   "M05.869", #  Other rheumatoid arthritis with rheumatoid factor of unspecified knee
#   "M05.87", #  Other rheumatoid arthritis with rheumatoid factor of ankle and foot
#   "M05.871", #  Other rheumatoid arthritis with rheumatoid factor of right ankle and foot
#   "M05.872", #  Other rheumatoid arthritis with rheumatoid factor of left ankle and foot
#   "M05.879", #  Other rheumatoid arthritis with rheumatoid factor of unspecified ankle and foot
#   "M05.89", #  …… of multiple sites
#   "M05.8A", #  …… of other specified site
#   "M05.9", #  Rheumatoid arthritis with rheumatoid factor, unspecified
#   "M06", #  Other rheumatoid arthritis
#   "M06.0", #  Rheumatoid arthritis without rheumatoid factor
#   "M06.00", #  …… unspecified site
#   "M06.01", #  Rheumatoid arthritis without rheumatoid factor, shoulder
#   "M06.011", #  Rheumatoid arthritis without rheumatoid factor, right shoulder
#   "M06.012", #  Rheumatoid arthritis without rheumatoid factor, left shoulder
#   "M06.019", #  Rheumatoid arthritis without rheumatoid factor, unspecified shoulder
#   "M06.02", #  Rheumatoid arthritis without rheumatoid factor, elbow
#   "M06.021", #  Rheumatoid arthritis without rheumatoid factor, right elbow
#   "M06.022", #  Rheumatoid arthritis without rheumatoid factor, left elbow
#   "M06.029", #  Rheumatoid arthritis without rheumatoid factor, unspecified elbow
#   "M06.03", #  Rheumatoid arthritis without rheumatoid factor, wrist
#   "M06.031", #  Rheumatoid arthritis without rheumatoid factor, right wrist
#   "M06.032", #  Rheumatoid arthritis without rheumatoid factor, left wrist
#   "M06.039", #  Rheumatoid arthritis without rheumatoid factor, unspecified wrist
#   "M06.04", #  Rheumatoid arthritis without rheumatoid factor, hand
#   "M06.041", #  Rheumatoid arthritis without rheumatoid factor, right hand
#   "M06.042", #  Rheumatoid arthritis without rheumatoid factor, left hand
#   "M06.049", #  Rheumatoid arthritis without rheumatoid factor, unspecified hand
#   "M06.05", #  Rheumatoid arthritis without rheumatoid factor, hip
#   "M06.051", #  Rheumatoid arthritis without rheumatoid factor, right hip
#   "M06.052", #  Rheumatoid arthritis without rheumatoid factor, left hip
#   "M06.059", #  Rheumatoid arthritis without rheumatoid factor, unspecified hip
#   "M06.06", #  Rheumatoid arthritis without rheumatoid factor, knee
#   "M06.061", #  Rheumatoid arthritis without rheumatoid factor, right knee
#   "M06.062", #  Rheumatoid arthritis without rheumatoid factor, left knee
#   "M06.069", #  Rheumatoid arthritis without rheumatoid factor, unspecified knee
#   "M06.07", #  Rheumatoid arthritis without rheumatoid factor, ankle and foot
#   "M06.071", #  Rheumatoid arthritis without rheumatoid factor, right ankle and foot
#   "M06.072", #  Rheumatoid arthritis without rheumatoid factor, left ankle and foot
#   "M06.079", #  Rheumatoid arthritis without rheumatoid factor, unspecified ankle and foot
#   "M06.08", #  …… vertebrae
#   "M06.09", #  …… multiple sites
#   "M06.0A", #  …… other specified site
#   "M06.1", #  Adult-onset Still's disease
#   "M06.2", #  Rheumatoid bursitis
#   "M06.20", #  …… unspecified site
#   "M06.21", #  Rheumatoid bursitis, shoulder
#   "M06.211", #  Rheumatoid bursitis, right shoulder
#   "M06.212", #  Rheumatoid bursitis, left shoulder
#   "M06.219", #  Rheumatoid bursitis, unspecified shoulder
#   "M06.22", #  Rheumatoid bursitis, elbow
#   "M06.221", #  Rheumatoid bursitis, right elbow
#   "M06.222", #  Rheumatoid bursitis, left elbow
#   "M06.229", #  Rheumatoid bursitis, unspecified elbow
#   "M06.23", #  Rheumatoid bursitis, wrist
#   "M06.231", #  Rheumatoid bursitis, right wrist
#   "M06.232", #  Rheumatoid bursitis, left wrist
#   "M06.239", #  Rheumatoid bursitis, unspecified wrist
#   "M06.24", #  Rheumatoid bursitis, hand
#   "M06.241", #  Rheumatoid bursitis, right hand
#   "M06.242", #  Rheumatoid bursitis, left hand
#   "M06.249", #  Rheumatoid bursitis, unspecified hand
#   "M06.25", #  Rheumatoid bursitis, hip
#   "M06.251", #  Rheumatoid bursitis, right hip
#   "M06.252", #  Rheumatoid bursitis, left hip
#   "M06.259", #  Rheumatoid bursitis, unspecified hip
#   "M06.26", #  Rheumatoid bursitis, knee
#   "M06.261", #  Rheumatoid bursitis, right knee
#   "M06.262", #  Rheumatoid bursitis, left knee
#   "M06.269", #  Rheumatoid bursitis, unspecified knee
#   "M06.27", #  Rheumatoid bursitis, ankle and foot
#   "M06.271", #  Rheumatoid bursitis, right ankle and foot
#   "M06.272", #  Rheumatoid bursitis, left ankle and foot
#   "M06.279", #  Rheumatoid bursitis, unspecified ankle and foot
#   "M06.28", #  …… vertebrae
#   "M06.29", #  …… multiple sites
#   "M06.3", #  Rheumatoid nodule
#   "M06.30", #  …… unspecified site
#   "M06.31", #  Rheumatoid nodule, shoulder
#   "M06.311", #  Rheumatoid nodule, right shoulder
#   "M06.312", #  Rheumatoid nodule, left shoulder
#   "M06.319", #  Rheumatoid nodule, unspecified shoulder
#   "M06.32", #  Rheumatoid nodule, elbow
#   "M06.321", #  Rheumatoid nodule, right elbow
#   "M06.322", #  Rheumatoid nodule, left elbow
#   "M06.329", #  Rheumatoid nodule, unspecified elbow
#   "M06.33", #  Rheumatoid nodule, wrist
#   "M06.331", #  Rheumatoid nodule, right wrist
#   "M06.332", #  Rheumatoid nodule, left wrist
#   "M06.339", #  Rheumatoid nodule, unspecified wrist
#   "M06.34", #  Rheumatoid nodule, hand
#   "M06.341", #  Rheumatoid nodule, right hand
#   "M06.342", #  Rheumatoid nodule, left hand
#   "M06.349", #  Rheumatoid nodule, unspecified hand
#   "M06.35", #  Rheumatoid nodule, hip
#   "M06.351", #  Rheumatoid nodule, right hip
#   "M06.352", #  Rheumatoid nodule, left hip
#   "M06.359", #  Rheumatoid nodule, unspecified hip
#   "M06.36", #  Rheumatoid nodule, knee
#   "M06.361", #  Rheumatoid nodule, right knee
#   "M06.362", #  Rheumatoid nodule, left knee
#   "M06.369", #  Rheumatoid nodule, unspecified knee
#   "M06.37", #  Rheumatoid nodule, ankle and foot
#   "M06.371", #  Rheumatoid nodule, right ankle and foot
#   "M06.372", #  Rheumatoid nodule, left ankle and foot
#   "M06.379", #  Rheumatoid nodule, unspecified ankle and foot
#   "M06.38", #  …… vertebrae
#   "M06.39", #  …… multiple sites
#   "M06.4", #  Inflammatory polyarthropathy
#   "M06.8", #  Other specified rheumatoid arthritis
#   "M06.80", #  …… unspecified site
#   "M06.81", #  Other specified rheumatoid arthritis, shoulder
#   "M06.811", #  Other specified rheumatoid arthritis, right shoulder
#   "M06.812", #  Other specified rheumatoid arthritis, left shoulder
#   "M06.819", #  Other specified rheumatoid arthritis, unspecified shoulder
#   "M06.82", #  Other specified rheumatoid arthritis, elbow
#   "M06.821", #  Other specified rheumatoid arthritis, right elbow
#   "M06.822", #  Other specified rheumatoid arthritis, left elbow
#   "M06.829", #  Other specified rheumatoid arthritis, unspecified elbow
#   "M06.83", #  Other specified rheumatoid arthritis, wrist
#   "M06.831", #  Other specified rheumatoid arthritis, right wrist
#   "M06.832", #  Other specified rheumatoid arthritis, left wrist
#   "M06.839", #  Other specified rheumatoid arthritis, unspecified wrist
#   "M06.84", #  Other specified rheumatoid arthritis, hand
#   "M06.841", #  Other specified rheumatoid arthritis, right hand
#   "M06.842", #  Other specified rheumatoid arthritis, left hand
#   "M06.849", #  Other specified rheumatoid arthritis, unspecified hand
#   "M06.85", #  Other specified rheumatoid arthritis, hip
#   "M06.851", #  Other specified rheumatoid arthritis, right hip
#   "M06.852", #  Other specified rheumatoid arthritis, left hip
#   "M06.859", #  Other specified rheumatoid arthritis, unspecified hip
#   "M06.86", #  Other specified rheumatoid arthritis, knee
#   "M06.861", #  Other specified rheumatoid arthritis, right knee
#   "M06.862", #  Other specified rheumatoid arthritis, left knee
#   "M06.869", #  Other specified rheumatoid arthritis, unspecified knee
#   "M06.87", #  Other specified rheumatoid arthritis, ankle and foot
#   "M06.871", #  Other specified rheumatoid arthritis, right ankle and foot
#   "M06.872", #  Other specified rheumatoid arthritis, left ankle and foot
#   "M06.879", #  Other specified rheumatoid arthritis, unspecified ankle and foot
#   "M06.88", #  …… vertebrae
#   "M06.89", #  …… multiple sites
#   "M06.8A", #  …… other specified site
#   "M06.9", #  Rheumatoid arthritis, unspecified
#   "M30", #  Polyarteritis nodosa and related conditions
#   "M30.0", #  Polyarteritis nodosa
#   "M30.1", #  Polyarteritis with lung involvement [Churg-Strauss]
#   "M30.2", #  Juvenile polyarteritis
#   "M30.3", #  Mucocutaneous lymph node syndrome [Kawasaki]
#   "M30.8", #  Other conditions related to polyarteritis nodosa
#   "M31", #  Other necrotizing vasculopathies
#   "M31.0", #  Hypersensitivity angiitis
#   "M31.1", #  Thrombotic microangiopathy
#   "M31.10", #  …… unspecified
#   "M31.11", #  Hematopoietic stem cell transplantation-associated thrombotic microangiopathy [HSCT-TMA]
#   "M31.19", #  Other thrombotic microangiopathy
#   "M31.2", #  Lethal midline granuloma
#   "M31.3", #  Wegener's granulomatosis
#   "M31.30", #  …… without renal involvement
#   "M31.31", #  …… with renal involvement
#   "M31.4", #  Aortic arch syndrome [Takayasu]
#   "M31.5", #  Giant cell arteritis with polymyalgia rheumatica
#   "M31.6", #  Other giant cell arteritis
#  "M31.7", #  Microscopic polyangiitis
#   "M31.8", #  Other specified necrotizing vasculopathies
#  "M31.9", #  Necrotizing vasculopathy, unspecified
#   "M32", #  Systemic lupus erythematosus (SLE)
#  "M32.0", #  Drug-induced systemic lupus erythematosus
#   "M32.1", #  Systemic lupus erythematosus with organ or system involvement
#  "M32.10", #  Systemic lupus erythematosus, organ or system involvement unspecified
#   "M32.11", #  Endocarditis in systemic lupus erythematosus
#  "M32.12", #  Pericarditis in systemic lupus erythematosus
#   "M32.13", #  Lung involvement in systemic lupus erythematosus
#  "M32.14", #  Glomerular disease in systemic lupus erythematosus
#   "M32.15", #  Tubulo-interstitial nephropathy in systemic lupus erythematosus
#  "M32.19", #  Other organ or system involvement in systemic lupus erythematosus
#   "M32.8", #  Other forms of systemic lupus erythematosus
#  "M32.9", #  Systemic lupus erythematosus, unspecified
#   "M33", #  Dermatopolymyositis
#  "M33.0", #  Juvenile dermatomyositis
#   "M33.00", #  …… organ involvement unspecified
#  "M33.01", #  …… with respiratory involvement
#   "M33.02", #  …… with myopathy
#   "M34", #  Systemic sclerosis [scleroderma]
#   "M34.0", #  Progressive systemic sclerosis
#  "M34.1", #  CR(E)ST syndrome
#   "M34.2", #  Systemic sclerosis induced by drug and chemical
#  "M34.8", #  Other forms of systemic sclerosis
#   "M34.81", #  Systemic sclerosis with lung involvement
#  "M34.82", #  Systemic sclerosis with myopathy
#   "M34.83", #  Systemic sclerosis with polyneuropathy
#  "M34.89", #  Other systemic sclerosis
#   "M34.9", #  Systemic sclerosis, unspecified
#  "M33.03", #  …… without myopathy
#   "M33.09", #  …… with other organ involvement
#  "M33.1", #  Other dermatomyositis
#   "M33.10", #  …… organ involvement unspecified
#  "M33.11", #  …… with respiratory involvement
#   "M33.12", #  …… with myopathy
#  "M33.13", #  …… without myopathy
#   "M33.19", #  …… with other organ involvement
#  "M33.2", #  Polymyositis
#   "M33.20", #  …… organ involvement unspecified
#  "M33.21", #  …… with respiratory involvement
#   "M33.22", #  …… with myopathy
#  "M33.29", #  …… with other organ involvement
#   "M33.9", #  Dermatopolymyositis, unspecified
#  "M33.90", #  …… organ involvement unspecified
#   "M33.91", #  …… with respiratory involvement
#  "M33.92", #  …… with myopathy
#   "M33.93", #  …… without myopathy
#  "M33.99", #  …… with other organ involvement
#   "M35", #  Other systemic involvement of connective tissue
#  "M35.0", #  Sjögren syndrome
#   "M35.00", #  …… unspecified
#  "M35.01", #  …… with keratoconjunctivitis
#   "M35.02", #  …… with lung involvement
#  "M35.03", #  …… with myopathy
#   "M35.04", #  …… with tubulo-interstitial nephropathy
#  "M35.05", #  …… with inflammatory arthritis
#   "M35.06", #  …… with peripheral nervous system involvement
#  "M35.07", #  …… with central nervous system involvement
#   "M35.08", #  …… with gastrointestinal involvement
#  "M35.0A", #  …… with glomerular disease
#   "M35.0B", #  …… with vasculitis
#  "M35.0C", #  …… with dental involvement
#   "M35.09", #  …… with other organ involvement
#   "M35.1", #  Other overlap syndromes
#   "M35.2", #  Behçet's disease
#   "M35.3", #  Polymyalgia rheumatica
#   "M35.4", #  Diffuse (eosinophilic) fasciitis
#   "M35.5", #  Multifocal fibrosclerosis
#   "M35.6", #  Relapsing panniculitis [Weber-Christian]
#   "M35.7", #  Hypermobility syndrome
#   "M35.8", #  Other specified systemic involvement of connective tissue
#   "M35.81", #  Multisystem inflammatory syndrome
#   "M35.89", #  Other specified systemic involvement of connective tissue
#   "M35.9", #  Systemic involvement of connective tissue, unspecified
#   "M36", #  Systemic disorders of connective tissue in diseases classified elsewhere
#   "M36.0", #  Dermato(poly)myositis in neoplastic disease
#   "M36.1", #  Arthropathy in neoplastic disease
#   "M36.2", #  Hemophilic arthropathy
#   "M36.3", #  Arthropathy in other blood disorders
#   "M36.4", #  Arthropathy in hypersensitivity reactions classified elsewhere
#   "M36.8", #  Systemic disorders of connective tissue in other diseases classified elsewhere
#   "M45", #  Ankylosing spondylitis
#   "M45.0", #  Ankylosing spondylitis of multiple sites in spine
#   "M45.1", #  Ankylosing spondylitis of occipito-atlanto-axial region
#   "M45.2", #  Ankylosing spondylitis of cervical region
#   "M45.3", #  Ankylosing spondylitis of cervicothoracic region
#   "M45.4", #  Ankylosing spondylitis of thoracic region
#   "M45.5", #  Ankylosing spondylitis of thoracolumbar region
#   "M45.6", #  Ankylosing spondylitis lumbar region
#   "M45.7", #  Ankylosing spondylitis of lumbosacral region
#   "M45.8", #  Ankylosing spondylitis sacral and sacrococcygeal region
#   "M45.9", #  Ankylosing spondylitis of unspecified sites in spine
#   "M45.A", #  Non-radiographic axial spondyloarthritis
#   "M45.A0", #  …… of unspecified sites in spine
#   "M45.A1", #  …… of occipito-atlanto-axial region
#   "M45.A2", #  …… of cervical region
#   "M45.A3", #  …… of cervicothoracic region
#   "M45.A4", #  …… of thoracic region
#   "M45.A5", #  …… of thoracolumbar region
#   "M45.A6", #  …… of lumbar region
#   "M45.A7", #  …… of lumbosacral region
#   "M45.A8", #  …… of sacral and sacrococcygeal region
#   "M45.AB" #  …… of multiple sites in spine  
# )

nlp_asthma <- c (
  "asthma",
  "hyperresponsive",
  "reactive airway disease",
  "reactive airways disease"
)

nlp_bronchiectasis <- c(
  "bronchiectasis",
  "cystic fibrosis",
  "kartagener",
  "primary ciliary dyskinesia"
)

nlp_chest_wall <- c (
  "ankylosing spondylitis",
  "flatback",
  "kyphosis",
  "kyphoscoliosis",
  "lordosis",
  "morbid obesity",
  "obesity hypoventilation syndrome",
  "scoliosis"
)

nlp_copd <- c (
  "chronic bronchitis",
  "chronic obstructive pulmonary disease",
  "copd",
  "emphysema"
)

nlp_ild <- c (
  "dermatopolymyositis",
  "desquamative interstitial pneumonia",
  "diffuse parenchymal lung disease",
  "dpld",
  "fibrosing alveolitis",
  "hypersensitivity pneumonitis",
  "idiopathic interstitial pneumonia",
  "idiopathic pulmonary fibrosis",
  "interstitial fibrosis",
  "interstitial lung disease",
  "lymphangioleiomyomatosis",
  "lymphoid interstitial pneumonia",
  "nonspecific interstitial pneumonia",
  "nsip",
  "pulmonary fibrosis",
  "pulmonary langerhans cell histiocytosis",
  "radiation pneumonitis",
  "rbild",
  "respiratory bronchiolitis interstitial lung disease",
  "rheumatoid lung disease",
  "sarcoidosis",
  "sjogren",
  "systemic sclerosis",
  "uip",
  "usual interstitial pneumonia"
)

nlp_neuromuscular <- c (
  "amyotrophic lateral sclerosis",
  "inclusion body myositis",
  "inflammatory polyneuropathy",
  "guillain-barre",
  "lambert-eaton",
  "motor neuron disease",
  "muscular atrophy",
  "muscular dystrophy",
  "multiple sclerosis",
  "myasthenia gravis",
  "myopathy",
  "myotonia",
  "myotonic",
  "progressive bulbar palsy"
)

nlp_ph <- c (
  "chronic thromboembolic disease",
  "cor pulmonale",
  "cteph",
  "eisenmenger",
  "pulmonary arterial hypertension",
  "pulmonary hypertension",
  "pulmonary vascular disease"
)

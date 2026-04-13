expression_emphysema <- "(?i)emphysema|emphysematous|centrilobular|panlobular|paraseptal"

expression_honeycombing <- "(?i)honeycomb(ing|ed)?|subpleural\\s*cystic\\s*changes"

expression_reticulation <- "(?i)reticulation|reticular|intralobular\\s*lines|net-like"

expression_thickening <- "(?i)(bronchial|peribronchial|airway)\\s*(wall)?\\s*(thickening|thickened|prominence|prominent|fullness|cuffing|cuffed)"

expression_traction <- "(?i)traction\\s*bronchiectasis|traction\\s*bronchiolectasis"

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

interpretation_copd <- list (
  bronchial_wall_thick = "(?i)bronchial wall thick|peribronchial thicken|bronchial wall thicken",
  bullous = "(?i)bulla|bullae|bullous",
  chronic_bronchitis = "(?i)chronic bronchitis",
  chronic_obstructive = "(?i)chronic obstructive pulmonary disease|\\bcopd\\b",
  copd = "(?i)\\bcopd\\b",
  emphysema = "(?i)emphysema|emphysematous|centrilobular|panlobular|paraseptal|panacinar",
  flattening_diaphragm = "(?i)flatten.{0,10}diaphragm|diaphragm.{0,10}flatten|depressed diaphragm",
  hyperinflated = "(?i)hyperinflat|air trapping|increased lung volume|lung hyperexpan",
  hyperinflation = "(?i)hyperinflat|air trapping|increased lung volume|lung hyperexpan"
)

interpretation_ild <- list (
  alveolar_proteinosis = "(?i)alveolar proteinosis|pulmonary alveolar proteinosis|\\bpap\\b",
  connective_tissue = "(?i)connective tissue disease|CTD.{0,10}ILD|connective tissue.{0,10}lung",
  desquamative = "(?i)desquamative|\\bdip\\b",
  eosinophilic_pneumonia = "(?i)eosinophilic pneumonia|pulmonary eosinophilia",
  fibrosis = "(?i)fibrosis|fibrotic|fibrosing",
  honeycomb = "(?i)honeycomb|honeycombing",
  hypersensitivity = "(?i)hypersensitivity pneumonitis|\\bhp\\b|extrinsic allergic alveolitis",
  ild = "(?i)\\bild\\b|interstitial lung disease",
  interstitial_abnormality = "(?i)interstitial abnormalit",
  interstitial_cystic = "(?i)interstitial cystic",
  interstitial_disease = "(?i)interstitial disease",
  interstitial_nodularity = "(?i)interstitial nodularit",
  interstitial_pneumonia = "(?i)interstitial pneumonia|interstitial pneumonitis",
  ipf = "(?i)\\bipf\\b|idiopathic pulmonary fibrosis",
  langerhans = "(?i)langerhans|pulmonary langerhans|\\bplch\\b",
  lam = "(?i)lymphangioleiomyomatosis|\\blam\\b",
  lip = "(?i)lymphoid interstitial pneumonia|\\blip\\b",
  nsip = "(?i)\\bnsip\\b|nonspecific interstitial pneumonia|non.specific interstitial pneumonia",
  organizing_pneumonia = "(?i)organizing pneumonia|\\bcop\\b|\\bboop\\b|bronchiolitis obliterans",
  pneumoconiosis = "(?i)pneumoconiosis|asbestosis|silicosis|coal worker",
  pulmonary_fibrosis = "(?i)pulmonary fibrosis",
  respiratory_bronchiolitis = "(?i)respiratory bronchiolitis|\\brb.ild\\b",
  reticular = "(?i)reticular|reticulation|reticulonodular",
  sarcoidosis = "(?i)sarcoidosis|sarcoid",
  traction_bronchiectasis = "(?i)traction bronchiectasis",
  uip = "(?i)\\buip\\b|usual interstitial pneumonia"
)

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

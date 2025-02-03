# 📚 kor-eng-r4ds: Book Reading 발표자료 및 코드 공유
R for Data Science (2nd edition) Practice in Korean &amp; English

## 개요
이 저장소는 **"R for Data Science"** 책의 특정 챕터에 대한 발표자료와 실습 코드를 공유하기 위한 공간입니다. 
각 파일은 `notebook`, `pdf`, `r_english`, `r_korean` 폴더에 정리되어 있으며, 챕터별로 구분됩니다.

## 폴더 구조
```
📂 notebook      # Jupyter Notebook 파일
    ├── 10-EDA.ipynb
    ├── 16-Factors.ipynb
    ├── 17-datetime.ipynb
    └── ...
📂 pdf           # Notebook을 변환한 PDF 파일
    ├── 10-EDA.pdf
    ├── 16-Factors.pdf
    ├── 17-datetime.pdf
    └── ...
📂 r_english     # 영어로 작성된 R 실습 코드
    ├── 10-EDA-script.r
    ├── 16-eng.r
    ├── 17-eng.r
    └── ...
📂 r_korean      # 한국어로 작성된 R 실습 코드
    ├── 16-kor.r
    ├── 17-kor.r
    └── ...
```

## 파일 설명
- **Notebook (`.ipynb`)**: 발표자료 및 실습 코드가 포함된 Jupyter Notebook 파일
- **PDF (`.pdf`)**: Notebook 파일을 PDF로 변환한 문서
- **R 코드 (`.r`)**: 해당 챕터 실습 코드 (영어 및 한국어 버전 제공)
- **환경 설정 파일 (`environment.yaml`)**: 실습 환경을 재현하기 위한 Conda 환경 설정 파일

## 챕터 목록
| 챕터 | 제목 |
|------|------------------------|
| 10   | Exploratory Data Analysis |
| 16   | Factors               |
| 17   | Dates and Times       |
| ...  | 추가된 챕터들         |

## 환경 설정
이 프로젝트의 실습 환경을 동일하게 설정하려면 `environment.yaml` 파일을 사용하여 Conda 환경을 생성할 수 있습니다.
```sh
conda env create -f environment.yaml
conda activate r-data-science
```

## 연락처
📧 **Nayoung Ku**  
📩 **nayoungku1@gmail.com**  
📅 **January 31, 2025**  

---

# 📚 Book Reading Presentation Materials & Code Sharing

## Overview
This repository contains presentation materials and practice code for specific chapters of **"R for Data Science."**
Files are organized into folders: `notebook`, `pdf`, `r_english`, and `r_korean`, with chapter-wise separation.

## Folder Structure
```
📂 notebook      # Jupyter Notebook files
    ├── 10-EDA.ipynb
    ├── 16-Factors.ipynb
    ├── 17-datetime.ipynb
    └── ...
📂 pdf           # Converted PDF files from Notebooks
    ├── 10-EDA.pdf
    ├── 16-Factors.pdf
    ├── 17-datetime.pdf
    └── ...
📂 r_english     # R practice code in English
    ├── 16-eng.r
    ├── 17-eng.r
    └── ...
📂 r_korean      # R practice code in Korean
    ├── 10-EDA-script.r
    ├── 16-kor.r
    ├── 17-kor.r
    └── ...
```

## File Descriptions
- **Notebook (`.ipynb`)**: Jupyter Notebook files with presentation content and practice code.
- **PDF (`.pdf`)**: Converted PDF versions of the Notebooks.
- **R Code (`.r`)**: Practice code files (available in both English and Korean).
- **Environment File (`environment.yaml`)**: Conda environment configuration file to reproduce the working environment.

## Chapter List
| Chapter | Title |
|---------|------------------------|
| 10      | Exploratory Data Analysis |
| 16      | Factors               |
| 17      | Dates and Times       |
| ...     | Additional Chapters   |

## Environment Setup
To replicate the same working environment for this project, use the `environment.yaml` file with Conda:
```sh
conda env create -f environment.yaml
conda activate r-data-science
```

## Contact
📧 **Nayoung Ku**  
📩 **nayoungku1@gmail.com**  
📅 **January 31, 2025**  

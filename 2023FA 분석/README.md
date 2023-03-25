# 2023 FA Transaction

> KBO FA 데이터 분석

<br>

**💡 분석 목표**

- 2023시즌 FA 대상자들의 시장에서의 가치 예측

| 목차                                                    | 내용                                                         |
| ------------------------------------------------------- | ------------------------------------------------------------ |
| [1. EDA]                                                | KBO 역대 FA 대상자들의 데이터            |
| [2. Multiple Linear regression]                         | 다중선형회귀모델을 사용해 시장에서의 FA금액 예측                 |
| [3. 포수WAR 보정 후 재예측]                              |포수 sWAR(STAIZ WAR) + 프레이밍을 통한 추가 승수로 WAR 보정, 금액 재예측 |

<br>

## 1. EDA

## Data 설명
---

역대 KBO 리그 FA 데이터(2023년까지)

- 총 188개의 관측치, 15개의 변수
- 결측치 없음
- 해외 리턴 사례, 다년 계약은 제외
- 부상으로 인한 시즌OUT-> 나머지 연도 평균값으로 대체

|칼럼명 | 설명|
|:---|:---|
| X1 Pos | 포지션(SP:선발투수, RP:불펜투수, IA:코너 내야, IB: 키스톤, O: 외야수) |
| X2 Name | 선수명 |
| X2 Team | 잔류시 원소속팀명, 이적시 ex) 삼성-> SSG |
| X3 Year | 계약연수 상세 ex) 4+2, 1+1 |
| X4 Salary | 옵션포함 총액 |
| X5 Start_age | FA연도 기준 만 나이 |
| X6 3b | 3년전 sWAR |
| X7 2b | 2년전 sWAR |
| X8 1b | 1년전 sWAR |
| X9 3y_war | 3년간 war 합계 |
| X10 Year_sum | 총 계약연수 |
| X11 Salary/yearly_sum | 연평균액 |
| X12 rank | 정성평가한 선수들의 시장에서의 등급, 4개의 levels('S', 'A', 'B', 'C')|
| Y Salary/yearly | 연평균액을 소수점 둘째 자리에서 반올림 |

> 2012~22시즌 FA 계약자-> train data # 167명
> 2023시즌 FA 대상자 -> test data # 21명

-변수간 상관계수

![상관관계](https://user-images.githubusercontent.com/63768509/227696344-198b945f-d562-4c28-a8ab-cdf44d05a958.jpg)
<br>

## 2. Multiple Linear regression


# Modelagem Conjunta de Dados Longitudinais e Riscos Competitivos 🚜📊

Repositório contendo os scripts analíticos e a metodologia computacional desenvolvidos para a dissertação de Mestrado no Programa de Pós-Graduação em Estatística e Ciência de Dados da Universidade Federal da Bahia (UFBA).

## Sobre a Pesquisa
A confiabilidade de equipamentos de grande porte é um desafio central na engenharia de manutenção. Este estudo propõe uma abordagem de **Modelagem Conjunta Bayesiana** para avaliar a dependência entre a trajetória longitudinal de degradação da banda de rodagem e o tempo até a falha sob um cenário de riscos competitivos em pneus *Off-The-Road* (OTR).

A inferência estatística foi construída sob o paradigma Bayesiano, utilizando Aproximações de Laplace Aninhadas Integradas (INLA) por meio do pacote `INLAjoint` no software R.

## 💎 Diferenciais do Projeto
* **Riqueza Empírica (Dados Reais):** O estudo utiliza uma base de dados histórica autêntica e complexa de uma operação de mineração a céu aberto no Chile, superando as tradicionais aplicações com dados puramente teóricos ou biomédicos.
* **Diagnóstico de Limitações Marginais:** O trabalho faz uma síntese comparativa profunda, provando matematicamente como a separação dos modelos falha em capturar o viés de endogeneidade, e como o Modelo Conjunto resolve esse problema.
* **Estudo de Simulação:** Validação das propriedades assintóticas e da robustez dos estimadores do modelo conjunto sob diferentes cenários de censura.

## Estrutura do Repositório
* 📁 `data/`: Diretório ignorado pelo controle de versão (protegido por sigilo industrial).
* 📁 `scripts/`: Códigos R estruturados numericamente (ETL, AED, Submodelos e Joint Model).
* 📁 `outputs/`: Tabelas e gráficos de diagnóstico reprodutíveis.
* 📁 `docs/`: Documentação de suporte.

## Principais Tecnologias
* **Linguagem:** R
* **Pacotes:** `INLA`, `INLAjoint`, `survival`, `nlme`, `gtsummary`, `tidyverse`.

## Autor
**Fernando Oliveira Bispo**  
Mestrando em Estatística e Ciência de Dados - UFBA
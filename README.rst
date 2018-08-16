=======================================================
Tópicos Especiais em Programação: Programação funcional
=======================================================

Este é o Git da disciplina Tópicos Especiais em Programação: Programação 
Funcional. Aqui será  o material produzido em sala de aula assim como tarefas,
wiki e discussões. Este arquivo contêm informações básicas sobre a disciplina e 
o plano de ensino do semestre.


Informações básicas
===================

Curso: 
    Engenharias
Professor: 
    Fábio Macêdo Mendes
Disciplina: 
    Tópicos Especiais em Programação: Programação funcional
Semestre/ano: 
    02/2018
Carga horária: 
    60 h
Créditos: 
    04


Ementa
======

* Princípios de programação funcional
* Composição de funções
* Curying e aplicação parcial
* Tipos imutáveis
* Tipos algébricos
* Mônadas


Horário das aulas e atendimento
===============================

Aulas teóricas e de exercícios: terças e quintas-feiras às 16h
Sala: FGA-I7
Atendimento: LAPPIS


Informações importantes
=======================

Este curso utiliza GitHub para gerenciar o curso. A comunicação com a turma é 
feita através de issues no repositório do Github. Habilite a funcionalidade 
"Watch" no repositório para receber notificações sobre atualizações.

Github:
    http://github.com/fabiommendes/programacao-funcional/


Critérios de avaliação
======================

A avaliação consiste em 1 prova, 2 trabalhos e exercícios segundo a 
proporção:

* Prova final: 30%
* Trabalhos: total de 50%, dividido nos pontos de controle
    * PC1: 15%
    * PC2: 25%
    * PC3: 60%
* Exercícios: 20%

Onde temos P1 representa as provas, T1 e T2 são os trabalhos e ET é a soma dos 
exercícios.

Prova substitutiva e faltas
---------------------------

O aluno pode faltar até 8 vezes em um semestre. Faltas com justificativa médica 
não serão abonadas, exceto em casos excepcionais. Os alunos reprovados por 
falta ficarão com uma menção igual a SR.

Código de ética e conduta
-------------------------

Algumas avaliações serão realizadas com auxílio do computador no laboratório de 
informática. Todas as submissões serão processadas por um programa de detecção 
de plágio. Qualquer atividade onde for detectada a presença de plágio será 
anulada sem a possibilidade de substituição. Não será feita qualquer distinção 
entre o aluno que forneceu a resposta para cópia e o aluno que obteve a mesma.


Prepare-se
==========

O curso utiliza alguns pacotes Python para os quais cada estudante deverá 
providenciar a instalação o mais cedo o possível. O curso requer Python 3.6+, 
ELM e Haskell: 

* Python: Versão 3.6+ com o pip.
* ELM: Instale a linguagem ELM ou o NPM.
* Haskell: instale o ghc na sua distribuição. Também disponibilizarei um 
    ambiente Haskell em Docker.

Linux e Docker
--------------

Os comandos de instalação acima assumem uma distribuição de Linux baseada em 
Debian como o Ubuntu ou o Mint. Não é necessário instalar uma distribuição 
deste tipo e você pode adaptar os comandos para o gerenciador de pacotes da sua 
distribuição (ou o Brew, no caso do OS X).


Cronograma de atividades
========================

+--------+-------+-------------------------------------------+
| Semana | Data  |                   Aula                    |
+========+=======+===========================================+
| 1      | 14/08 | Início das aulas – Apresentação do curso  |
|        |       |                                           |
|        |       | * Funcional vs Imperativo                 |
|        |       | * Python funcional                        |
+--------+-------+-------------------------------------------+
|        | 16/08 | Python funcional com sidekick             |
|        |       |                                           |
|        |       | * Pipeline de funções                     |
|        |       | * Composição de funções                   |
|        |       | * Aplicação parcial e curying             |
+--------+-------+-------------------------------------------+
| 2      | 21/08 | ADTs                                      |
|        |       |                                           |
|        |       | * Tipos algébricos em Python              |
|        |       | * Ágebra de tipos                         |
|        |       | * Maybe e Result                          |
+--------+-------+-------------------------------------------+
|        | 23/08 | Controle de erros funcional               |
|        |       |                                           |
|        |       | * Maybe                                   |
|        |       | * Result                                  |
+--------+-------+-------------------------------------------+
| 3      | 28/08 | Listas                                    |
|        |       |                                           |
|        |       | * Listas como ADTs                        |
|        |       | * Listas simplemente encadeadas           |
|        |       | * Estruturas de dados imutáveis           |
+--------+-------+-------------------------------------------+
|        | 30/08 | Laços e repetições                        |
|        |       |                                           |
|        |       | * Compreensão de listas                   |
|        |       | * Map e reduce                            |
|        |       | * Recursão                                |
+--------+-------+-------------------------------------------+
| 4      | 04/09 | ELM                                       |
|        |       |                                           |
|        |       | * Introdução ao ELM                       |
|        |       | * Gerando HTML                            |
|        |       | * Sintaxe e tipos básicos                 |
+--------+-------+-------------------------------------------+
|        | 06/09 | Tipos em ELM                              |
|        |       |                                           |
|        |       | * "Records" e "union types"               |
|        |       | * Expressões condicionais                 |
|        |       | * Tipos básicos como ADTs                 |
|        |       | * Desestruturação                         |
+--------+-------+-------------------------------------------+
| 5      | 11/09 | Funções em ELM                            |
|        |       |                                           |
|        |       | * Auto-curying                            |
|        |       | * Lambdas                                 |
|        |       | * Operadores de funções                   |
|        |       | * Assinatura de funções                   |
+--------+-------+-------------------------------------------+
|        | 13/09 | Arquitetura Elm (TEA)                     |
|        |       |                                           |
|        |       | * Modelos e Mensagens,                    |
|        |       | * View e update                           |
|        |       | * Exemplo: "TODO List"                    |
+--------+-------+-------------------------------------------+
| 6      | 11/09 | Jogos em ELM                              |
|        |       |                                           |
|        |       | * Canvas                                  |
|        |       | * Pong                                    |
+--------+-------+-------------------------------------------+
|        | 13/09 | Jogos em ELM                              |
|        |       |                                           |
|        |       | * Continuação...                          |
+--------+-------+-------------------------------------------+
| 7      | 18/09 | JSON                                      |
|        |       |                                           |
|        |       | * JSON vs linguagens estáticas            |
|        |       | * Convertendo para JSON                   |
|        |       | * Lendo JSON                              |
+--------+-------+-------------------------------------------+
|        | 20/09 | Consumindo uma API                        |
|        |       |                                           |
|        |       | * Lendo uma API                           |
|        |       | * Enviando e recebendo requisições        |
+--------+-------+-------------------------------------------+
| 8      | 25/09 | Semana Universitária                      |
+--------+-------+-------------------------------------------+
|        | 27/09 |                                           |
+--------+-------+-------------------------------------------+
| 9      | 02/10 | Estruturas de dados imutáveis             |
|        |       |                                           |
|        |       | * Operações com listas                    |
|        |       | * Processamento de listas                 |
+--------+-------+-------------------------------------------+
|        | 04/10 | Implementando uma estrutura de dados      |
|        |       |                                           |
|        |       | * Fita                                    |
+--------+-------+-------------------------------------------+
| 10     | 09/10 | Publicando pacotes ELM                    |
|        |       |                                           |
|        |       | * Estrutura de módulos                    |
|        |       | * Documentação                            |
|        |       | * Elm package                             |
+--------+-------+-------------------------------------------+
|        | 11/10 | Interação com Javascript                  |
|        |       |                                           |
|        |       | * Sistema de "ports"                      |
|        |       | * Interagindo com bibliotecas JS          |
|        |       | * HTML keyed e lazy                       |
+--------+-------+-------------------------------------------+
| 11     | 16/10 | Intepretador Brainfuck                    |
|        |       |                                           |
|        |       | * Lendo entrada do usuário                |
|        |       | * Manipulando a fita                      |
|        |       | * Loops                                   |
+--------+-------+-------------------------------------------+
|        | 18/10 | Interpretador Brainfuck                   |
|        |       |                                           |
|        |       | * Continuação...                          |
+--------+-------+-------------------------------------------+
| 12     | 23/10 | Introdução ao Haskell                     |
|        |       |                                           |
|        |       | * Histórico                               |
|        |       | * Sintaxe do Haskell                      |
|        |       | * Haskell vs. ELM (principais diferenças) |
+--------+-------+-------------------------------------------+
|        | 25/10 | Funções                                   |
|        |       |                                           |
|        |       | * Pattern matching e guardas              |
|        |       | * Recursão em Haskell                     |
|        |       | * Otimização para recursão de cauda       |
+--------+-------+-------------------------------------------+
| 13     | 30/10 | Haskell prática                           |
|        |       |                                           |
|        |       | * Scotty                                  |
|        |       | * Servidor de API simples                 |
+--------+-------+-------------------------------------------+
|        | 01/11 | Módulos                                   |
|        |       |                                           |
|        |       | * Carregando módulos                      |
|        |       | * Criando módulos                         |
+--------+-------+-------------------------------------------+
| 14     | 06/11 | Recursão                                  |
|        |       |                                           |
|        |       | * Recursão em Haskell                     |
|        |       | * Otimização para recursão de cauda       |
+--------+-------+-------------------------------------------+
|        | 08/11 | Sistema de tipos                          |
|        |       |                                           |
|        |       | * Anotações de tipos                      |
|        |       | * Derivação                               |
|        |       | * Estruturas de dados recursivas          |
+--------+-------+-------------------------------------------+
| 15     | 13/11 | Typeclasses                               |
|        |       |                                           |
|        |       | * Derivação de classes                    |
|        |       | * Functores                               |
|        |       | * Criando próprias classes                |
+--------+-------+-------------------------------------------+
|        | 15/11 | **Feriado:** Proclamação da República     |
+--------+-------+-------------------------------------------+
| 16     | 20/11 | IO                                        |
|        |       |                                           |
|        |       | * "Hello World"                           |
|        |       | * Notação "do"                            |
|        |       | * Entrada e saída                         |
+--------+-------+-------------------------------------------+
|        | 22/11 | Modelando efeitos                         |
|        |       |                                           |
|        |       | * Contexto de uma computação              |
|        |       | * Functores                               |
|        |       | * Aplicativos                             |
|        |       | * Monóides                                |
+--------+-------+-------------------------------------------+
| 17     | 27/11 | Mônadas                                   |
|        |       |                                           |
|        |       | * Maybe e List                            |
|        |       | * Lidando com contexto                    |
|        |       | * Composição de funções                   |
|        |       | * Operadores de mônadas                   |
+--------+-------+-------------------------------------------+
|        | 29/11 | Leis Monádicas                            |
|        |       |                                           |
|        |       | * Leis de Mônadas                         |
|        |       | * Notação "do" e mônadas                  |
|        |       | * Mônadas em todos os lugares             |
+--------+-------+-------------------------------------------+
| 18     | 04/12 | Prova                                     |
+--------+-------+-------------------------------------------+
|        | 06/12 | Apresentação dos projetos                 |
+--------+-------+-------------------------------------------+


Obs.: O cronograma está sujeito a alterações.

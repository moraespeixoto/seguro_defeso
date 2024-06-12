## Carregar pacotes e banco do censo ----
pacman::p_load(tidyverse,haven,ggthemes,sjPlot, lme4, wordcloud,
               glmmTMB, performance,gtsummary,labelled, tm)
# 4. Modelos de regressao ----

### 4.1 Modelo acesso ao seguro defeso ----


load("~/seguro_defeso/MULHERES_DEFESO/banco_modelo.Rda")


modelo_2 <- glmer(Seguro_defeso ~
                    Feminino + `Cor (branco)` + #  Acesso a internet +
                    #  Participa Pescarte 
                    + Idade + `Escolaridade (faixas)` +
                    `Renda familiar per capita` + (1|municipio),
                  data = banco_modelo, family = binomial(link = "logit") )


modelo_3 <- glmer(Seguro_defeso ~
                    Feminino + `Cor (branco)` + #  Acesso a internet +
                    #  Participa Pescarte 
                    + Idade + `Escolaridade (faixas)` +
                    `Renda familiar per capita` + (1|municipio),
                  data = banco_modelo, family = binomial(link = "logit") )

modelo_4 <- glmer(Seguro_defeso ~
                    Feminino + `Cor (branco)` + #  Acesso a internet +
                    #  Participa Pescarte 
                    + Idade + `Escolaridade (faixas)` +
                    log_renda_familiar + (1|municipio),
                  data = banco_modelo, family = binomial(link = "logit") )


tab_model(modelo_2)

plot_model(modelo_2)

plot_model(modelo_2,
           type = "re")

table(banco_modelo$Seguro_defeso)

### 4.2 Modelo problema de documentacao ----

modelo_3 <- glm(Documentação ~
                Feminino + `Cor (branco)` + # `Três ou mais filhos` + `Acesso a internet` + `Participa Pescarte` +
                Idade + `Escolaridade (faixas)` + `Renda familiar per capita`,
                data = subset(banco_modelo, Seguro_defeso == 0 ),
                family = binomial(link = "logit") )

modelo_4 <- glmer(Documentação ~ Feminino + `Cor (branco)` + # Três ou mais filhos + Acesso a internet + Participa Pescarte +
                    Idade + `Escolaridade (faixas)` + `Renda familiar per capita` + (1|municipio),
                  data = subset(banco_modelo, Seguro_defeso == 0 ), family = binomial(link = "logit") )

tab_model(modelo_2, modelo_4)

( plot_model <- plot_models(modelo_2, modelo_4, line.size = 1.3,
            m.labels = c(
              "Recebimento do Defeso", "Obstáculo de documentação"))+
  theme_minimal()+
  theme(legend.position  = "bottom",
        legend.key.width = unit(2, "lines"),  # Largura da chave da legenda
        legend.key.height = unit(2, "lines"),  # Altura da chave da legenda
        axis.text.x = element_text(size =17),
        axis.text.y = element_text(size =19),
        plot.background = element_rect(fill = "white"),
        legend.text = element_text(size = 16))  +
  labs( title = "",
        fill = "",
        col = "",
        x = "",
        y = "")+
  guides(fill= "none") )

ggsave(plot_model, file = "~/censo_da_pesca_pea_pescarte/MULHERES_DEFESO/plots/plot_model.png",
       height = 11, width = 16, dpi = 600)

save(modelo, file = "~/censo_da_pesca_pea_pescarte/MULHERES_DEFESO/modelo.Rda")

tbl_regression(modelo, exp = T)

##

plot_model(modelo_4,
           type = "re")+
  theme_minimal()+
  theme(legend.position  = "bottom",
        legend.key.width = unit(2, "lines"),  # Largura da chave da legenda
        legend.key.height = unit(2, "lines"),  # Altura da chave da legenda
        axis.text.x = element_text(size =17),
        axis.text.y = element_text(size =19),
        plot.background = element_rect(fill = "white"),
        legend.text = element_text(size = 16))  +
  labs( title = "Modelo 2: Obstáculo de documentação",
        fill = "",
        col = "",
        x = "",
        y = "")+
  guides(fill= "none")

###

pred_1  <- plot_model(modelo_2, type = "pred",
           terms = c("Renda familiar per capita","Feminino"))+
  theme_minimal()+
  theme(legend.position  = "bottom",
        legend.key.width = unit(2, "lines"),  # Largura da chave da legenda
        legend.key.height = unit(2, "lines"),  # Altura da chave da legenda
        axis.text.x = element_text(size =17),
        axis.text.y = element_text(size =19),
        plot.background = element_rect(fill = "white"),
        legend.text = element_text(size = 16))  +
   guides(fill= "none")+ 
   scale_color_manual(values = c("0" = "blue", "1" = "red")) +  # Definindo as cores das linhas
  scale_fill_manual(values = c("0" = "royalblue", "1" = "firebrick"))  # Definindo as cores do erro padrão 

pred_2 <- plot_model(modelo_4, type = "pred",
           terms = c("Renda familiar per capita","Feminino"))+
  theme_minimal()+
  theme(legend.position  = "bottom",
        legend.key.width = unit(2, "lines"),  # Largura da chave da legenda
        legend.key.height = unit(2, "lines"),  # Altura da chave da legenda
        axis.text.x = element_text(size =17),
        axis.text.y = element_text(size =19),
        plot.background = element_rect(fill = "white"),
        legend.text = element_text(size = 16))  +
  guides(fill= "none")+
  scale_color_manual(values = c("0" = "blue", "1" = "red")) +  # Definindo as cores das linhas
  scale_fill_manual(values = c("0" = "royalblue", "1" = "firebrick"))  # Definindo as cores do erro padrão

valores_preditos <- gridExtra::grid.arrange(pred_1, pred_2, nrow = 2)

ggsave(valores_preditos, file = "~/censo_da_pesca_pea_pescarte/MULHERES_DEFESO/plots/valores_preditos.png",
       height = 11, width = 16, dpi = 600)

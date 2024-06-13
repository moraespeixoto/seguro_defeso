## Carregar pacotes e banco do censo ----

pacman::p_load(tidyverse,haven,ggthemes,sjPlot, lme4, wordcloud,
               glmmTMB, performance,gtsummary,labelled, tm)

load("~/seguro_defeso/MULHERES_DEFESO/banco_modelo.Rda")


## 1. grafico de obstáculos por genero ---- 

censo_obstaculos <- banco_modelo |> 
  select(CF.4, TTP.53_1, TTP.53_5, TTP.53_9) |> 
  mutate(Feminino = ifelse(CF.4 == 2, 1, 0),
         TTP.53_5 = ifelse(TTP.53_5 == 5, 1, 0),
         TTP.53_9 = ifelse(TTP.53_9 == 9, 1, 0)) |> 
  mutate_at(vars(TTP.53_1,TTP.53_5,TTP.53_9), ~ifelse(is.na(.), 0, TRUE))

banco_ttp1 <- censo_obstaculos  |> 
  rename(Resposta =  TTP.53_1) |> 
  mutate(Gênero = ifelse(CF.4 == 1, "Masculino", "Feminino")) |>  
  group_by(Gênero, Resposta) |> 
  summarise(total_genero_obstaculo = n(),
  ) |> 
  mutate(n = sum(total_genero_obstaculo,  na.rm = T),
         p = total_genero_obstaculo/n,
         erro = sqrt((p *(1 - p)/n)),
         perc = p *100,
         p_erro = erro * 100) |> 
  filter(Resposta == 1) |> 
  mutate(tipo = "Registro de pescador")

banco_ttp2 <- censo_obstaculos |> 
  rename(Resposta = TTP.53_5) |> 
  mutate(Gênero = ifelse(CF.4 == 1, "Masculino", "Feminino")) |> 
  group_by(Gênero, Resposta) |> 
  summarise(total_genero_obstaculo = n(),
  ) |> 
  mutate(n = sum(total_genero_obstaculo,  na.rm = T),
         p = total_genero_obstaculo/n,
         erro = sqrt((p *(1 - p)/n)),
         perc = p *100,
         p_erro = erro * 100) |> 
  filter(Resposta == 1) |> 
  mutate(tipo = "Dificuldade de requerimento")

banco_ttp9 <- censo_obstaculos  |> 
  rename(Resposta =  TTP.53_9) |> 
  mutate(Gênero = ifelse(CF.4 == 1, "Masculino", "Feminino")) |>  
  group_by(Gênero, Resposta) |> 
  summarise(total_genero_obstaculo = n(),
  ) |> 
  mutate(n = sum(total_genero_obstaculo,  na.rm = T),
         p = total_genero_obstaculo/n,
         erro = sqrt((p *(1 - p)/n)),
         perc = p *100,
         p_erro = erro * 100)  |>
  filter(Resposta == 1) |> 
  mutate(tipo = "Insegurança por perda de benefícios")

obstaculos <- bind_rows(banco_ttp1,
                        banco_ttp2,
                        banco_ttp9)

( plot_1 <- obstaculos |> 
    ggplot(aes(fill = as_factor(Gênero), y = perc, x = as_factor(tipo)), color = "black")+
    geom_bar(stat = "identity", position = position_dodge(width = 0.8),
             width = 0.7, color = "black")+
    geom_errorbar(aes(ymin = perc - p_erro, ymax = perc + p_erro),
                  position = position_dodge(0.8), size = 1.2, width = .3, color = "darkgray")+
    scale_fill_manual(values =  c("lightblue", "steelblue"))+
    labs(title = "",
         fill = "",
         y = "%",
         x = "")+
    theme_minimal()+
    theme(plot.background = element_rect(fill = "white"),
          title = element_text(size = 14),
          axis.title.y = element_text(size = 18, family = "times"),
          axis.text.y = element_text(size = 20, family = "times"),
          axis.text.x = element_text(size = 20, family = "times"),
          legend.text = element_text(size = 22, family = "times"),
          plot.caption = element_text(hjust = 0,size = 13, family = "times"),
          legend.box = "horizontal",
          legend.position = "bottom",
    ) )

## 2. Descritiva: proporção de homens e mulheres que recebem o defeso ---- 

censo_genero_defeso <- banco_modelo |> 
  filter(TTP.40 >= 1) |> 
  mutate(Gênero = ifelse(CF.4 == 1, "Masculino", "Feminino")) |> 
  group_by(Gênero, TTP.40) |> 
  summarise(total_genero_ttp_40 = n(),
  ) |> 
  mutate(n = sum(total_genero_ttp_40,  na.rm = T),
         p = total_genero_ttp_40/n,
         erro = sqrt((p *(1 - p)/n)),
         perc = p *100,
         p_erro = erro * 100) |> 
  mutate(perc = round(perc, 2))

( plot_2 <- censo_genero_defeso |> 
    ggplot()+
    aes(fill = as_factor(Gênero), y = perc, x = as_factor(Gênero))+
    geom_bar(stat = "identity", position ="dodge", color = "black")+
    geom_errorbar(aes(ymin = perc - p_erro, ymax = perc + p_erro),
                  position = position_dodge(0.8), size = 1.2, width = .3, color = "darkgray")+
    scale_fill_manual(values =  c("lightblue", "steelblue"))+
    ylim(0,40)+
    labs(fill = "",
         y = "%",
         x = "",
         caption = "")+
    theme_minimal()+
    theme(plot.background = element_rect(fill = "white"),
          axis.title.y = element_text(size = 18, family = "times"),
          axis.text.y = element_text(size = 20, family = "times"),
          axis.text.x = element_text(size = 20, family = "times"),
          legend.text = element_text(size = 22, family = "times"),
          legend.position = "bottom",
          legend.key = element_rect(fill = "white", color = "white"),
          strip.background=element_rect(fill="white", colour="white")) ) 

## 3. Por genero: Problemas relacionados a documentos, entre os que nao recebem ----

### 3.1 Tratamento: nuvem de palavras ----

ttp_40.1 <- Corpus(VectorSource(banco_modelo$TTP.40.1))

ttp_40.1 <- tm_map(ttp_40.1, content_transformer(tolower)) # Converta texto para minúsculas
ttp_40.1 <- tm_map(ttp_40.1, removePunctuation) # Remova pontuações
ttp_40.1 <- tm_map(ttp_40.1, removeNumbers) # Remova números
ttp_40.1 <- tm_map(ttp_40.1, removeWords, stopwords("portuguese")) # Remova stopwords
ttp_40.1 <- tm_map(ttp_40.1, stripWhitespace) # Remova espaços em branco extras

wordcloud(words = ttp_40.1,
          min.freq = 1,
          random.order = FALSE,
          colors = brewer.pal(8, "Dark2"))


censo_filtrado <- banco_modelo |> 
  select(CF.4, TTP.40.1) |> na.omit(T) |>  
  mutate(defeso = ifelse(grepl(c("documento|documentos|documentação"),
                               TTP.40.1, ignore.case = TRUE), 1, 0))


cf4_defeso <- censo_filtrado  |> 
  mutate(Gênero = ifelse(CF.4 == 1, "Masculino", "Feminino")) |>  
  group_by(Gênero, defeso) |> 
  summarise(total_genero_ttp_401 = n(),
  ) |> 
  mutate(n = sum(total_genero_ttp_401,  na.rm = T),
         p = total_genero_ttp_401/n,
         erro = sqrt((p *(1 - p)/n)),
         perc = p *100,
         p_erro = erro * 100) |> 
  mutate(perc = round(perc, 2))

cf4_defeso |> 
  filter(defeso == 1) |> 
  ggplot(
    aes(fill = as_factor(Gênero), y = perc, x = Gênero), color = "black")+
  geom_bar(stat = "identity", position ="dodge", color = "black")+
  geom_text(aes(label = paste0(format(perc, nsmall = 1, decimal.mark = ","), "%")),
            vjust = 3.5, size = 5) +
  geom_errorbar(aes(ymin = perc - p_erro, ymax = perc + p_erro),
                position = position_dodge(0.8), size = 1.2, width = .3, color = "darkgray")+
  scale_fill_manual(values =  c("lightblue", "steelblue"))+
  ylim(0,60)+
  labs(fill = "",
       y = "%",
       x = "",
       caption = "")+
  theme_minimal()+
  theme(
    axis.title.y = element_text(size = 12, family = "times"),
    axis.text.y = element_text(size = 12, family = "times"),
    axis.text.x = element_text(size = 12, family = "times"),
    legend.text = element_text(size = 14, family = "times"),
    legend.position = "bottom",
    legend.key = element_rect(fill = "white", color = "white"),
    strip.background=element_rect(fill="white", colour="white"))


# 4. Modelos de regressao ----

### 4.1 Modelo acesso ao seguro defeso ----

modelo_1 <- glmer(Seguro_defeso ~
                    Feminino + `Cor (branco)` + #  Acesso a internet +
                    #  Participa Pescarte 
                    + Idade + `Escolaridade (faixas)` +
                    `Renda familiar per capita` + (1|municipio),
                  data = banco_modelo, family = binomial(link = "logit") )



tab_model(modelo_1)

plot_model(modelo_1)

plot_model(modelo_1,
           type = "re")

table(banco_modelo$Seguro_defeso)

### 4.2 Modelo problema de documentacao ----

modelo_2 <- glmer(Documentação ~ Feminino + `Cor (branco)` + # Três ou mais filhos + Acesso a internet + Participa Pescarte +
                    Idade + `Escolaridade (faixas)` + `Renda familiar per capita` + (1|municipio),
                  data = subset(banco_modelo, Seguro_defeso == 0 ), family = binomial(link = "logit") )

tab_model(modelo_1, modelo_2)

( plot_model <- plot_models(modelo_1, modelo_2, line.size = 1.3,
            m.labels = c(
              "Recebimento do Defeso", "Obstáculo de documentação"))+
    ylim(0.2, 2)+
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

plot_model(modelo_2,
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

pred_1  <- plot_model(modelo_1, type = "pred",
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

pred_2 <- plot_model(modelo_2, type = "pred",
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

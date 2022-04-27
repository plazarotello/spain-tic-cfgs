if (!require('tidyverse')) install.packages('tidyverse'); library('tidyverse')

#==============================================================================#
raw.age <- read.csv('edades.csv', stringsAsFactors=TRUE, header = TRUE,
                    dec = ',', sep = ';', fileEncoding = 'UTF-8-BOM')

age <- raw.age %>% rename(sexo = Sexo, edad = Edad,
                          TIC = Habilidades.TIC, porcentaje = Total) %>%
  filter(TIC != 'Total')
#==============================================================================#
raw.education <- read.csv('educacion.csv', stringsAsFactors=TRUE, header = TRUE,
                    dec = ',', sep = ';', fileEncoding = 'UTF-8-BOM')

education <- raw.education %>% rename(CCAA = CCAA.de.estudio, sexo = Sexo,
                                      educacion = Tipo.de.centro,
                                      TIC = Habilidades.TIC,
                                      titulados = Total) %>%
  mutate(titulados = as.numeric(sub('\\.', '', as.character(titulados))))
#==============================================================================#
raw.factors <- read.csv('factores.csv', stringsAsFactors=TRUE, header = TRUE,
                    dec = ',', sep = ';', fileEncoding = 'UTF-8-BOM')
factors <- raw.factors %>% rename(sexo = Sexo, tmp = Factores.que.han.influido.en.conseguir.trabajo,
                                  porcentaje = Total) %>%
  filter(tmp != 'Total trabajando cuenta ajena') %>% mutate(tmp = as.character(tmp)) %>%
  separate(col = tmp, sep = ' *: *', into = c('factor', 'nivel')) %>%
  mutate(factor = as.factor(factor), nivel = as.factor(nivel))
#==============================================================================#
raw.skills <- read.csv('habilidades.csv', stringsAsFactors=TRUE, header = TRUE,
                    dec = ',', sep = ';', fileEncoding = 'UTF-8-BOM')
skills <- raw.skills %>% rename(sexo = Sexo, subsector = Ciclo,
                                TIC = Habilidades.TIC, porcentaje = Total) %>%
  filter(TIC != 'Total')
levels(skills$subsector)
skills$subsector <- recode_factor(
  skills$subsector, '01 - AGRARIA' = 'Agricultura',
  'Técnico superior en gestión forestal y del medio natural' = 'Silvicultura',
  'TS en paisajismo y medio rural y TS Gestión y organización de empresas agropecuarias' = 'Silvicultura',
  '02 - MARÍTIMO-PESQUERA' = 'Pesca',
  '03 - INDUSTRIAS ALIMENTARIAS' = 'Industria de la alimentación', '04 - QUÍMICA' = 'Industria química',
  'Técnico superior en laboratorio de análisis y de control de calidad' = 'Industria química',
  'Técnico superior en química industrial Fabricación de Productos Farmaceuticos e Industrias de proceso de pasta y papel' = 'Industria química',
  '05 - IMAGEN PERSONAL' = 'Imagen personal', 'Técnico superior en estética integral y bienestar' = 'Imagen personal',
  'TS en estilismo y dirección de peluquería y TS en asesoría de imagen personal y corporativa' = 'Imagen personal',
  '06 - SANIDAD' = 'Sanidad', 'Técnico superior en audiología protésica' = 'Sanidad',
  'Técnico superior en prótesis dentales Higiene bucodental' = 'Sanidad',
  'Imagen para el diagnóstico y medicina nuclear Radioterapia y dosimetría' = 'Sanidad', 'Otros ciclos de Sanidad' = 'Sanidad',
  '07 - SEGURIDAD Y MEDIO AMBIENTE' = 'Seguridad', '08 - FABRICACIÓN MECÁNICA' = 'Mecánica',
  'Técnico superior en programación de la producción en fabricación mecánica' = 'Mecánica',
  'TS en construcciones metálicas TS en diseño en fabricación mecánica Programación de la producción en moldeo de metales y polímeros y Óptica de anteojería' = 'Metalurgia',
  '09 - INSTALACIÓN Y MANTENIMIENTO' = 'Instalación y mantenimiento', 'Técnico superior en mecatrónica industrial' = 'Mecatrónica',
  'TS en desarrollo de proyectos de instalaciones térmicas y de fluidos y TS en mantenimiento de instalaciones térmicas y de fluidos' = 'Instalación y mantenimiento',
  '10 - ELECTRICIDAD Y ELECTRÓNICA' = 'Electricidad y electrónica', 'Técnico superior en sistemas electrotécnicos y automatizados' = 'Electricidad y electrónica',
  'Técnico superior en sistemas de telecomunicaciones e informáticos' = 'Electricidad y electrónica',
  'Técnico superior en mantenimiento electrónico' = 'Electricidad y electrónica', 'Técnico superior en automatización y robótica industrial' = 'Electricidad y electrónica',
  '11 - ENERGÍA Y AGUA' = 'Energía y agua', '12 - TRANSPORTE Y MANTENIMIENTO DE VEHÍCULOS' = 'Transporte',
  '14 - EDIFICACIÓN Y OBRA CIVIL' = 'Construcción', 'Técnico superior en proyectos de edificación' = 'Construcción',
  'Técnico superior en proyectos de obra civil Y Organización y control de obras de construcción' = 'Construcción',
  '18 - ARTES GRÁFICAS' = 'Imagen y sonido', '19 - IMAGEN Y SONIDO' = 'Imagen y sonido',
  'Técnico superior en Producción de audiovisuales y espectáculos' = 'Imagen y sonido',
  'Técnico superior en sonido para audiovisuales y espectáculos' = 'Imagen y sonido',
  'Técnico superior en realización de proyectos de audiovisuales y espectáculos' = 'Imagen y sonido',
  'TS en animaciones 3D, juegos y entornos interactivos y TS en iluminación, captación y tratamiento de Imagen' = 'Imagen y sonido',
  '20 - INFORMÁTICA Y COMUNICACIONES' = 'Informática y comunicaciones', 'Técnico superior en administración de sistemas informáticos en red' = 'Informática y comunicaciones',
  'Técnico superior en desarrollo de aplicaciones multiplataforma' = 'Informática y comunicaciones',
  'Técnico superior en desarrollo de aplicaciones web' = 'Informática y comunicaciones', '21 - ADMINISTRACIÓN Y GESTIÓN' = 'Administración',
  'Técnico superior en asistencia a la dirección' = 'Administración', 'Técnico superior en administración y finanzas' = 'Administración',
  '22 - COMERCIO Y MARKETING' = 'Comercio y marketing', 'Técnico superior en comercio internacional' = 'Comercio y marketing',
  'Técnico superior en transporte y logística' = 'Transporte',
  'TS en gestión de ventas y espacios comerciales TS en marketing y publicidad y Servicios al consumidor' = 'Comercio y marketing',
  '23 - SERVICIOS SOCIOCULTURALES Y A LA COMUNIDAD' = 'Servicios sociales', 'Técnico superior en educación infantil' = 'Educación',
  'Técnico superior en animación sociocultural y turística' = 'Hostelería y turismo', 'Técnico superior en integración social' = 'Servicios sociales',
  'Técnico superior en promoción de igualdad de género Interpretación de Lenguajes de Signos' = 'Servicios sociales',
  '24 - HOSTELERÍA Y TURISMO' = 'Hostelería y turismo', 'Técnico superior en gestión de alojamientos turísticos' = 'Hostelería y turismo',
  'Técnico superior en guía, información y asistencias turísticas' = 'Hostelería y turismo',
  'Técnico superior en agencias de viajes y gestión de eventos' = 'Hostelería y turismo',
  'TS en dirección de cocina y TS en dirección de servicios de restauración' = 'Hostelería y turismo',
  '25 - ACTIVIDADES FÍSICAS Y DEPORTIVAS' = 'Deporte',
  '15 - VIDRIO Y CERÁMICA 16 - MADERA, MUEBLE Y CORCHO 17 - TEXTIL, CONFECCIÓN Y PIEL 26 - ARTES Y ARTESANÍAS' = 'Artesanía')
levels(skills$subsector)
skills <- skills %>% mutate(sector = case_when(subsector %in% c('Agricultura', 'Silvicultura', 'Pesca') ~ 'Primario',

                                               subsector %in% c('Industria de la alimentación', 'Industria química',
                                                                'Mecánica', 'Metalurgia', 'Instalación y mantenimiento',
                                                                'Mecatrónica', 'Artesanía', 'Construcción',
                                                                'Energía y agua', 'Electricidad y electrónica') ~'Secundario',

                                               subsector %in% c('Imagen personal', 'Sanidad', 'Seguridad',
                                                                'Transporte', 'Imagen y sonido', 'Administración',
                                                                'Informática y comunicaciones', 'Comercio y marketing',
                                                                'Servicios sociales', 'Educación', 'Deporte',
                                                                'Hostelería y turismo') ~'Terciario',
                                               TRUE ~ 'N/A')) %>%
  mutate(sector = as.factor(sector))
skills <- skills %>% group_by(sexo, subsector, sector, TIC) %>%
  summarise(porcentaje = round(mean(porcentaje), 2))
#==============================================================================#
raw.work <- read.csv('insercion-laboral.csv', stringsAsFactors=TRUE,
                     header = TRUE, dec = ',', sep = ';',
                     fileEncoding = 'UTF-8-BOM')
work <- raw.work %>% rename(sexo = Sexo, insercion = Inserción.laboral,
                                TIC = Habilidades.TIC, porcentaje = Total) %>%
  filter(insercion != 'Total') %>% filter(TIC != 'Total') %>%
  mutate(insercion=ifelse(insercion == 'Ha trabajado después de acabar el ciclo', TRUE, FALSE))
#==============================================================================#

write.csv(age, file = 'mod-edad.csv', row.names = FALSE)
write.csv(education, file = 'mod-educacion.csv', row.names = FALSE)
write.csv(factors, file = 'mod-factores.csv', row.names = FALSE)
write.csv(skills, file = 'mod-habilidades.csv', row.names = FALSE)
write.csv(work, file = 'mod-insercion.csv', row.names = FALSE)

#==============================================================================#

rm(raw.age, raw.education, raw.factors, raw.skills, raw.work)

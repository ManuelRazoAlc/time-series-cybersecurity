# Análisis completo del proyecto: simulación, modelos, detección y pronóstico.
# Lo ejecuta presentacion.qmd al renderizar; también corre solo desde la raíz
# del proyecto con: Rscript R/analisis.R
# Escribe los datos simulados en datos/ y los resultados en resultados/.

if (!requireNamespace("jsonlite", quietly=TRUE)) stop("Instale jsonlite con install.packages('jsonlite')")
for(d in c("datos","resultados")) dir.create(d,showWarnings=FALSE,recursive=TRUE)
options(stringsAsFactors=FALSE, digits=12)
SEEDS <- c(20261027L,20261028L,20261029L)
MODELS <- c("ARIMA","SARIMA","ETS","Isolation Forest")
SCENARIOS <- c("Normal","Pico","Sostenido","Rafagas","Benigno")
TRAIN_DAYS <- 84L; CAL_DAYS <- 18L; TEST_DAYS <- 18L
TRAIN <- TRAIN_DAYS*96L; CAL_END <- (TRAIN_DAYS+CAL_DAYS)*96L
N <- 120L*96L; PRIMARY <- SEEDS[1]
PAL <- c(ARIMA="#003D64",SARIMA="#0897D5",ETS="#E39918",`Isolation Forest`="#7F9297",`Ingenuo estacional`="#7F9297")
NAVY <- "#003D64"; AQUA <- "#0897D5"; RED <- "#E8442A"; GREY <- "#7F9297"
csv <- function(x,f) write.csv(x,file.path("resultados",f),row.names=FALSE,na="")
json <- function(x,f) jsonlite::write_json(x,file.path("resultados",f),auto_unbox=TRUE,digits=8,na="null",null="null",pretty=FALSE)

# 1. GENERACION. Cada observacion es un CONTEO agregado de cinco minutos.
# u_t=.7*u_{t-1}+eta_t, eta~N(0,.08^2), inicializacion estacionaria.
# lambda_t=exp(log(18)+.45*sin(2*pi*h/24)+.15*cos(4*pi*h/24)+u_t).
# X_t|lambda~Pois(lambda); M_t|lambda~Pois(.05*lambda+.2).
# X: intentos de acceso. M: alertas de un monitor previo comun a los modelos.
# Ambos tienen ruidos independientes condicionales a lambda.
make_base <- function(seed){
  set.seed(seed); n <- 120L*288L; i <- 0:(n-1L); hour <- (i*5+2.5)/60
  u <- numeric(n); u[1] <- rnorm(1,0,.08/sqrt(1-.7^2))
  eta <- rnorm(n,0,.08)
  for(t in 2:n) u[t] <- .7*u[t-1]+eta[t]
  intensity <- exp(log(18)+.45*sin(2*pi*hour/24)+.15*cos(4*pi*hour/24)+u)
  data.frame(i=i,start_min=i*5,end_min=(i+1)*5,lambda=intensity,
             activity=rpois(n,intensity),alerts=rpois(n,.05*intensity+.2))
}
make_events <- function(seed){
  peak <- data.frame(seed=seed,scenario="Pico",id=1:4,
    start_min=c(150370,156505,161650,168310),end_min=c(150380,156515,161660,168320),
    malicious=1L,intensity=c(35,60,45,85))
  sustained <- data.frame(seed=seed,scenario="Sostenido",id=1L,start_min=159010,
    end_min=172800,malicious=1L,intensity=12)
  bursts <- data.frame(seed=seed,scenario="Rafagas",id=1:8,
    start_min=150370+(0:7)*2880,end_min=150430+(0:7)*2880,
    malicious=1L,intensity=c(16,25,40,20,16,25,40,20))
  benign <- peak; benign$scenario <- "Benigno"; benign$malicious <- 0L
  rbind(peak,sustained,bursts,benign)
}
make_tests <- function(base,events,seed){
  baseline <- base[base$start_min>=102*1440,]; out <- list()
  for(sc in c("Normal","Pico","Sostenido","Rafagas")){
    z <- baseline; z$attack <- 0L; z$event <- 0L; z$extra <- 0L
    ev <- events[events$scenario==sc,]
    set.seed(seed+match(sc,SCENARIOS)*100L)
    for(k in seq_len(nrow(ev))){
      ix <- which(z$start_min<ev$end_min[k] & z$end_min>ev$start_min[k])
      # Los inicios estan alineados a los registros de cinco minutos.
      ex <- rpois(length(ix),ev$intensity[k])
      z$activity[ix] <- z$activity[ix]+ex
      z$alerts[ix] <- z$alerts[ix]+rpois(length(ix),.4*ex)
      z$extra[ix] <- z$extra[ix]+ex; z$attack[ix] <- 1L; z$event[ix] <- ev$id[k]
    }
    out[[sc]] <- z
  }
  out$Benigno <- out$Pico; out$Benigno$attack <- 0L
  stopifnot(identical(out$Pico$activity,out$Benigno$activity),identical(out$Pico$alerts,out$Benigno$alerts))
  out
}
aggregate_counts <- function(z,minutes){
  group <- z$start_min%/%minutes
  ans <- aggregate(cbind(activity,alerts)~group,data=transform(z,group=group),FUN=sum)
  ans$start_min <- ans$group*minutes; ans$end_min <- ans$start_min+minutes
  if("attack"%in%names(z)){
    lab <- aggregate(cbind(attack,event)~group,data=transform(z,group=group),FUN=max)
    ans <- merge(ans,lab,by="group",sort=TRUE)
  }
  ans
}

# 2. MODELOS. ARIMA y SARIMA se estiman por suma de cuadrados condicionales.
# Ordenes fijados antes de prueba; no se eligen usando etiquetas.
# Las potencias estacionales se expanden en fit$model$phi y $theta.
fit_model <- function(y,model,m){
  if(model=="ETS"){
    fit <- stats::HoltWinters(ts(y,frequency=m),beta=FALSE,gamma=NULL,seasonal="additive")
    state <- list(model=model,level=unname(fit$coefficients["a"]),
                  season=unname(fit$coefficients[paste0("s",1:m)]),j=1L,
                  alpha=fit$alpha,gamma=fit$gamma,m=m)
    residuals <- as.numeric(y[(length(y)-nrow(fit$fitted)+1):length(y)]-fit$fitted[,"xhat"])
    check <- max(abs(as.numeric(stats::predict(fit,n.ahead=48))-future(state,48)))
    if(check>1e-5) stop("La recurrencia ETS no coincide con predict.HoltWinters")
    return(list(fit=fit,state=state,residuals=residuals,forecast_check=check))
  }
  seasonal <- if(model=="SARIMA") list(order=c(1L,0L,0L),period=m) else list(order=c(0L,0L,0L),period=m)
  fit <- stats::arima(y,order=if(model=="SARIMA")c(1L,0L,1L) else c(2L,0L,1L),
    seasonal=seasonal,include.mean=TRUE,method="CSS",optim.control=list(maxit=600))
  mu <- if("intercept"%in%names(fit$coef)) unname(fit$coef["intercept"]) else 0
  p <- length(fit$model$phi); q <- length(fit$model$theta)
  state <- list(model=model,phi=fit$model$phi,theta=fit$model$theta,mu=mu,
    past_y=tail(y,max(1L,p)),past_e=tail(as.numeric(fit$residuals),max(1L,q)),m=m)
  # Independently compare recurrence with the library's one-step forecast.
  direct <- as.numeric(stats::predict(fit,n.ahead=48)$pred)
  own <- future(state,48)
  if(max(abs(direct-own))>1e-5) stop("La recurrencia ARMA no coincide con predict.arima: ",model)
  list(fit=fit,state=state,residuals=as.numeric(fit$residuals),forecast_check=max(abs(direct-own)))
}
# Use explicit additions: avoid precedence ambiguity in conditional expressions.
expected <- function(s){
  if(s$model=="ETS") return(s$level+s$season[s$j])
  p <- length(s$phi); q <- length(s$theta); value <- s$mu
  if(p) value <- value+sum(s$phi*rev(tail(s$past_y,p)-s$mu))
  if(q) value <- value+sum(s$theta*rev(tail(s$past_e,q)))
  value
}
update_state <- function(s,y){
  pred <- expected(s); err <- y-pred
  if(s$model=="ETS"){
    # This is the Holt-Winters update used by stats::HoltWinters.
    old_season <- s$season[s$j]
    s$level <- s$alpha*(y-old_season)+(1-s$alpha)*s$level
    s$season[s$j] <- s$gamma*(y-s$level)+(1-s$gamma)*old_season
    s$j <- s$j%%s$m+1L
  } else {
    s$past_y <- tail(c(s$past_y,y),max(1L,length(s$phi)))
    s$past_e <- tail(c(s$past_e,err),max(1L,length(s$theta)))
  }
  s
}
walk <- function(state,y){
  pred <- numeric(length(y))
  for(i in seq_along(y)){pred[i] <- expected(state);state <- update_state(state,y[i])}
  list(prediction=pred,state=state)
}
future <- function(state,h){
  pred <- numeric(h)
  for(i in seq_len(h)){pred[i] <- expected(state);state <- update_state(state,pred[i])}
  pmax(0,pred)
}

# 3. ISOLATION FOREST ORIGINAL. Arboles binarios aleatorios, no clasificador.
# Liu, Ting y Zhou (2008), score=2^(-E[h(x)]/c(psi)).
# La longitud incluye c(n) al terminar en una hoja con varios puntos.
c_path <- function(n){
  ans <- numeric(length(n)); ans[n==2] <- 1
  k <- n>2; ans[k] <- 2*(digamma(n[k])+0.5772156649015329)-2*(n[k]-1)/n[k]
  ans
}
tree_fit <- function(x,depth=0L,max_depth=8L){
  n <- nrow(x)
  if(n<=1L || depth>=max_depth) return(list(n=n))
  lo <- apply(x,2,min); hi <- apply(x,2,max); possible <- which(hi>lo)
  if(!length(possible)) return(list(n=n))
  q <- possible[sample.int(length(possible),1L)]; p <- runif(1,lo[q],hi[q]); left <- x[,q]<p
  if(all(left)||!any(left)) return(list(n=n))
  list(q=q,p=p,left=tree_fit(x[left,,drop=FALSE],depth+1L,max_depth),
       right=tree_fit(x[!left,,drop=FALSE],depth+1L,max_depth))
}
tree_path <- function(tree,x,depth=0L){
  if(!is.null(tree$n)) return(rep(depth+c_path(tree$n),nrow(x)))
  left <- x[,tree$q]<tree$p; ans <- numeric(nrow(x))
  if(any(left)) ans[left] <- tree_path(tree$left,x[left,,drop=FALSE],depth+1L)
  if(any(!left)) ans[!left] <- tree_path(tree$right,x[!left,,drop=FALSE],depth+1L)
  ans
}
fit_if <- function(x,seed,ntrees=200L,psi=256L){
  set.seed(seed+9000L); psi <- min(psi,nrow(x))
  list(trees=lapply(seq_len(ntrees),function(i)tree_fit(x[sample.int(nrow(x),psi),,drop=FALSE],0L,ceiling(log2(psi)))),psi=psi)
}
score_if <- function(fit,x){
  lengths <- numeric(nrow(x))
  for(tr in fit$trees) lengths <- lengths+tree_path(tr,x)
  2^(-(lengths/length(fit$trees))/c_path(fit$psi))
}
features <- function(y,end_min){
  # El dato actual solo se utiliza DESPUES del cierre, para detectar.
  # Nunca se incluye el dato futuro ni una etiqueta entre las entradas.
  lag1 <- c(y[1],head(y,-1)); lag4 <- c(rep(y[1],4),head(y,-4))
  phase <- 2*pi*((end_min-7.5)%%1440)/1440
  cbind(current=y,lag1=lag1,lag4=lag4,sin_day=sin(phase),cos_day=cos(phase))
}

# 4. EVALUACION POR INTERVALO Y POR EPISODIO.
metrics <- function(y,attack,alarm,ev,end_min,seed,scenario,model){
  tp <- sum(attack==1 & alarm); fp <- sum(attack==0 & alarm)
  fn <- sum(attack==1 & !alarm); tn <- sum(attack==0 & !alarm)
  precision <- if(tp+fp)tp/(tp+fp) else NA_real_
  recall <- if(tp+fn)tp/(tp+fn) else NA_real_
  rows <- list()
  malignant <- ev[ev$malicious==1,]
  for(i in seq_len(nrow(malignant))){
    ix <- which(end_min>malignant$start_min[i] & end_min-15<malignant$end_min[i])
    hits <- ix[alarm[ix]]; first <- if(length(hits))end_min[hits[1]] else NA_real_
    rows[[i]] <- data.frame(seed=seed,scenario=scenario,model=model,event=malignant$id[i],
      start_min=malignant$start_min[i],end_min=malignant$end_min[i],
      detected=length(hits)>0,first_alert_min=first,delay_min=first-malignant$start_min[i],
      affected_intervals=length(ix),flagged_intervals=length(hits),coverage=length(hits)/length(ix))
  }
  episodes <- if(length(rows))do.call(rbind,rows) else NULL
  events <- nrow(malignant); detected <- if(events)sum(episodes$detected) else 0L
  row <- data.frame(seed=seed,scenario=scenario,model=model,tp=tp,fp=fp,fn=fn,tn=tn,
    recall=recall,precision=precision,f1=if(2*tp+fp+fn>0)2*tp/(2*tp+fp+fn)else NA_real_,
    false_per_day=fp/TEST_DAYS,false_per_normal_day=fp/(sum(attack==0)/96),
    events=events,detected=detected,missed=events-detected,
    delay_median=if(detected)median(episodes$delay_min[episodes$detected])else NA_real_)
  list(row=row,episodes=episodes)
}

all_det <- list(); all_episode <- list(); all_fore <- list(); all_fm <- list(); all_diag <- list(); all_param <- list(); all_thresholds <- list()
bases <- list(); tests <- list(); detections <- list(); hourly <- list(); forecasts_primary <- list(); events_all <- list()
resid_primary <- list(); sensitivity_thresholds <- list(); anticipation <- list()
cat("Generando datos y ajustando modelos desde cero...\n")
for(seed in SEEDS){
  cat("Semilla",seed,"\n"); key <- as.character(seed)
  base <- make_base(seed); ev <- make_events(seed); test <- make_tests(base,ev,seed)
  bases[[key]] <- base;tests[[key]] <- test; events_all[[key]] <- ev
  write.csv(base,file.path("datos",paste0("base_5min_",seed,".csv")),row.names=FALSE)
  for(sc in SCENARIOS)write.csv(test[[sc]],file.path("datos",paste0("prueba_5min_",seed,"_",sc,".csv")),row.names=FALSE)
  b15 <- aggregate_counts(base,15); yy <- b15$activity
  train_y <- yy[seq_len(TRAIN)]; cal_y <- yy[(TRAIN+1):CAL_END]
  xtrain <- features(train_y,b15$end_min[seq_len(TRAIN)])[-(1:4),,drop=FALSE]
  forest <- fit_if(xtrain,seed)
  xall <- features(yy,b15$end_min)
  cal_if <- score_if(forest,xall[(TRAIN+1):CAL_END,,drop=FALSE])
  fitted <- setNames(lapply(MODELS[1:3],function(m)fit_model(train_y,m,96L)),MODELS[1:3])
  cal <- lapply(fitted,function(f)walk(f$state,cal_y))
  thresholds <- c(vapply(MODELS[1:3],function(m)unname(quantile(abs(cal_y-cal[[m]]$prediction),.99,type=1)),0),
                  `Isolation Forest`=unname(quantile(cal_if,.99,type=1)))
  for(m in MODELS){
    scores <- if(m=="Isolation Forest")cal_if else abs(cal_y-cal[[m]]$prediction)
    for(prob in c(.95,.975,.99,.995))all_thresholds[[length(all_thresholds)+1]] <- data.frame(seed=seed,model=m,q=prob,threshold=unname(quantile(scores,prob,type=1)))
  }
  for(m in MODELS[1:3]){
    r <- fitted[[m]]$residuals;df <- if(m=="ARIMA")3L else if(m=="SARIMA")3L else 0L
    all_diag[[length(all_diag)+1]] <- data.frame(seed=seed,task="detection",model=m,lag=96L,
      ljung_p=Box.test(r,lag=96,type="Ljung-Box",fitdf=df)$p.value,
      forecast_check=fitted[[m]]$forecast_check)
    if(seed==PRIMARY)resid_primary[[paste0("detection_",m)]] <- r
    co <- fitted[[m]]$fit$coef
    if(m=="ETS")co <- c(alpha=fitted[[m]]$fit$alpha,gamma=fitted[[m]]$fit$gamma)
    all_param[[length(all_param)+1]] <- data.frame(seed=seed,task="detection",model=m,param=names(co),value=unname(co))
  }
  dseed <- list()
  for(sc in SCENARIOS){
    z <- aggregate_counts(test[[sc]],15); obs <- z$activity; mods <- list()
    for(m in MODELS){
      if(m=="Isolation Forest"){
        combined <- c(yy[1:CAL_END],obs)
        x <- features(combined,c(b15$end_min[1:CAL_END],z$end_min))
        score <- score_if(forest,tail(x,nrow(z)));pred <- rep(NA_real_,length(obs))
      }else{
        w <- walk(cal[[m]]$state,obs);pred <- w$prediction;score <- abs(obs-pred)
      }
      alarm <- score>thresholds[m]
      mm <- metrics(obs,z$attack,alarm,ev[ev$scenario==sc,],z$end_min,seed,sc,m)
      all_det[[length(all_det)+1]] <- mm$row
      if(!is.null(mm$episodes))all_episode[[length(all_episode)+1]] <- mm$episodes
      dt <- data.frame(seed=seed,scenario=sc,model=m,end_min=z$end_min,observed=obs,
        attack=z$attack,event=z$event,prediction=pred,score=score,threshold=unname(thresholds[m]),alert=as.integer(alarm))
      csv(dt,paste0("deteccion_",seed,"_",sc,"_",gsub(" ","_",m),".csv"))
      mods[[m]] <- list(expected=round(pred,5),score=round(score,6),threshold=unname(thresholds[m]),alarm=as.integer(alarm),metrics=as.list(mm$row[1,]))
    }
    dseed[[sc]] <- list(x=z$end_min/60,y=obs,attack=z$attack,event=z$event,models=mods)
  }
  detections[[key]] <- dseed
  # Contrafactual exacto: mismos datos, puntuaciones y decisiones. Solo cambia verdad.
  stopifnot(identical(dseed$Pico$y,dseed$Benigno$y))
  for(m in MODELS)stopifnot(identical(dseed$Pico$models[[m]]$alarm,dseed$Benigno$models[[m]]$alarm))
  # 5. PRONOSTICO DE CARGA NORMAL DEL MONITOR, NO DE ATAQUES INDIVIDUALES.
  hr <- aggregate_counts(base,60); y <- hr$alerts;hourly[[key]] <- list(x=hr$end_min/60,y=y)
  write.csv(data.frame(end_min=hr$end_min,alerts=y),file.path("datos",paste0("alertas_horarias_",seed,".csv")),row.names=FALSE)
  tr <- 84L*24L;ce <- 102L*24L; horizon <- 48L
  fm <- setNames(lapply(MODELS[1:3],function(m)fit_model(y[1:tr],m,24L)),MODELS[1:3])
  for(m in MODELS[1:3]){
    r <- fm[[m]]$residuals
    all_diag[[length(all_diag)+1]] <- data.frame(seed=seed,task="forecast",model=m,lag=48L,
      ljung_p=Box.test(r,lag=48,type="Ljung-Box",fitdf=if(m=="ETS")0L else 3L)$p.value,
      forecast_check=fm[[m]]$forecast_check)
    if(seed==PRIMARY)resid_primary[[paste0("forecast_",m)]] <- r
    co <- fm[[m]]$fit$coef
    if(m=="ETS")co <- c(alpha=fm[[m]]$fit$alpha,gamma=fm[[m]]$fit$gamma)
    all_param[[length(all_param)+1]] <- data.frame(seed=seed,task="forecast",model=m,param=names(co),value=unname(co))
  }
  origins_cal <- seq(tr,ce-horizon,by=6L);origins_test <- seq(ce,length(y)-horizon,by=6L)
  origin_all <- sort(unique(c(origins_cal,origins_test)))
  if(seed==PRIMARY){
    surprise_origin <- floor(min(ev$start_min[ev$scenario=="Pico"])/360)*6L
    attacked_for_example <- c(y[1:ce],aggregate_counts(test$Pico,60)$alerts)
    stopifnot(identical(y[1:surprise_origin],attacked_for_example[1:surprise_origin]))
    xx <- (surprise_origin-24L):(surprise_origin+48L)
    anticipation <- list(origin=surprise_origin,onset=min(ev$start_min[ev$scenario=="Pico"])/60,
      x=xx,normal=y[xx],attacked=attacked_for_example[xx],models=list())
  }
  for(m in c(MODELS[1:3],"Ingenuo estacional")){
    current <- if(m!="Ingenuo estacional")fm[[m]]$state else NULL
    rows <- list(); current_i <- tr
    for(o in origin_all){
      if(m!="Ingenuo estacional" && o>current_i){
        current <- walk(current,y[(current_i+1):o])$state;current_i <- o
      }
      pp <- if(m=="Ingenuo estacional")rep(y[(o-23L):o],length.out=horizon)else future(current,horizon)
      set <- if(o%in%origins_test)"test" else "calibration"
      rows[[length(rows)+1]] <- data.frame(seed=seed,model=m,set=set,origin=o,h=1:horizon,
        observed=y[o+(1:horizon)],prediction=pp)
    }
    pred <- do.call(rbind,rows)
    # Cada horizonte tiene una banda MARGINAL calibrada solo en pasado normal.
    # La correccion por rango finito no garantiza cobertura bajo dependencia.
    widths <- vapply(1:horizon,function(h){
      e <- pred[pred$set=="calibration" & pred$h==h,]; v <- abs(e$observed-e$prediction)
      as.numeric(quantile(v,min(1,ceiling((length(v)+1)*.9)/length(v)),type=1))
    },0)
    pred$lower <- pmax(0,pred$prediction-widths[pred$h]);pred$upper <- pred$prediction+widths[pred$h]
    pred$scenario <- "Normal"
    all_fore[[length(all_fore)+1]] <- pred
    for(h in c(1L,6L,24L,48L)){
      a <- pred[pred$set=="test" & pred$h==h,]
      all_fm[[length(all_fm)+1]] <- data.frame(seed=seed,scenario="Normal",model=m,h=h,n=nrow(a),
        mae=mean(abs(a$observed-a$prediction)),rmse=sqrt(mean((a$observed-a$prediction)^2)),
        coverage=mean(a$observed>=a$lower&a$observed<=a$upper),mean_width=mean(a$upper-a$lower))
    }
    # Irrupcion inesperada: monitor con pulsos de ataque. Todo el pasado
    # anterior al episodio coincide exactamente con el escenario normal.
    attacked_y <- c(y[1:ce],aggregate_counts(test$Pico,60)$alerts)
    current_attack <- if(m!="Ingenuo estacional")walk(fm[[m]]$state,attacked_y[(tr+1):ce])$state else NULL
    current_i_attack <- ce; attack_rows <- list()
    for(o in origins_test){
      if(m!="Ingenuo estacional" && o>current_i_attack){
        current_attack <- walk(current_attack,attacked_y[(current_i_attack+1):o])$state
        current_i_attack <- o
      }
      pp <- if(m=="Ingenuo estacional")rep(attacked_y[(o-23L):o],length.out=horizon)else future(current_attack,horizon)
      attack_rows[[length(attack_rows)+1]] <- data.frame(seed=seed,model=m,set="test",origin=o,h=1:horizon,
        observed=attacked_y[o+(1:horizon)],prediction=pp,
        lower=pmax(0,pp-widths),upper=pp+widths,scenario="Sorpresa")
    }
    attack_pred <- do.call(rbind,attack_rows)
    all_fore[[length(all_fore)+1]] <- attack_pred
    for(h in c(1L,6L,24L,48L)){
      a <- attack_pred[attack_pred$h==h,]
      all_fm[[length(all_fm)+1]] <- data.frame(seed=seed,scenario="Sorpresa",model=m,h=h,n=nrow(a),
        mae=mean(abs(a$observed-a$prediction)),rmse=sqrt(mean((a$observed-a$prediction)^2)),
        coverage=mean(a$observed>=a$lower&a$observed<=a$upper),mean_width=mean(a$upper-a$lower))
    }
    if(seed==PRIMARY){
      ex_state <- if(m!="Ingenuo estacional")walk(fm[[m]]$state,y[(tr+1):surprise_origin])$state else NULL
      ex <- if(m=="Ingenuo estacional")rep(y[(surprise_origin-23L):surprise_origin],length.out=horizon)else future(ex_state,horizon)
      anticipation$models[[m]] <- list(x=surprise_origin+1:horizon,prediction=round(ex,5),lower=round(pmax(0,ex-widths),5),upper=round(ex+widths,5))
      forecasts_primary[[m]] <- setNames(lapply(origins_test,function(o){
        z <- pred[pred$origin==o & pred$set=="test",]
        list(x=o+z$h,prediction=round(z$prediction,5),lower=round(z$lower,5),upper=round(z$upper,5))
      }),as.character(origins_test))
    }
  }
}
det <- do.call(rbind,all_det); episodes <- do.call(rbind,all_episode); fore <- do.call(rbind,all_fore)
fmetric <- do.call(rbind,all_fm);diagnostics <- do.call(rbind,all_diag);events <- do.call(rbind,events_all)
csv(det,"metricas_deteccion.csv");csv(episodes,"retrasos_por_episodio.csv");csv(fore,"pronosticos_rolling.csv")
csv(fmetric,"metricas_pronostico.csv");csv(diagnostics,"diagnosticos.csv");csv(do.call(rbind,all_param),"parametros.csv")
csv(do.call(rbind,all_thresholds),"umbrales.csv")
write.csv(events,file.path("datos","eventos.csv"),row.names=FALSE)

# Tablas resumen: cada semilla es una realizacion de UN proceso generador.
# Minimo y maximo no representan intervalos de confianza.
forecast_mean <- aggregate(cbind(mae,rmse,coverage)~model+h,data=fmetric[fmetric$scenario=="Normal",],FUN=mean)
forecast_min <- aggregate(mae~model+h,data=fmetric[fmetric$scenario=="Normal",],FUN=min);names(forecast_min)[3] <- "mae_min"
forecast_max <- aggregate(mae~model+h,data=fmetric[fmetric$scenario=="Normal",],FUN=max);names(forecast_max)[3] <- "mae_max"
forecast_mean <- merge(merge(forecast_mean,forecast_min),forecast_max)
det_mean <- aggregate(cbind(recall,false_per_day,detected,events)~scenario+model,data=det,FUN=mean,na.action=na.pass)
csv(forecast_mean,"resumen_pronostico.csv");csv(det_mean,"resumen_deteccion.csv")
summary <- list(meta=list(primary_seed=PRIMARY,seeds=SEEDS,days=120,split_days=c(84,18,18),
    base_minutes=5,detection_minutes=15,forecast_minutes=60,threshold_quantile=.99,
    forecast_band=.9,forecast_origin_step_hours=6,time_convention="Horas y dias transcurridos desde cero",
    dgp=list(baseline=18,daily_sin=.45,daily_cos2=.15,ar1=.7,innovation_sd=.08,monitor_rate=.05,monitor_floor=.2,attack_monitor_multiplier=.4)),
  detection_metrics=det,episode_metrics=episodes,forecast_metrics=fmetric,forecast_mean=forecast_mean,
  detection_mean=det_mean,diagnostics=diagnostics,
  caveats=c("Simulacion propia: no son resultados calculados en la revision.",
    "Tres semillas de un mismo proceso no validan superioridad universal.",
    "Pronostico de carga normal del monitor; no predice ataques individuales.",
    "Isolation Forest solo detecta y no participa en pronostico.",
    "Intervalos de 15 minutos: las falsas alertas no equivalen a tickets operativos."))
json(summary,"resumen.json")
jsonlite::write_json(summary$meta,file.path("datos","parametros_simulacion.json"),auto_unbox=TRUE,pretty=TRUE)
write.csv(data.frame(variable=c("i","start_min","end_min","lambda","activity","alerts","attack","event","extra"),
 descripcion=c("Indice de registro desde cero","Inicio del intervalo en minutos transcurridos","Cierre del intervalo en minutos transcurridos","Intensidad latente Poisson de actividad","Conteo observado de intentos de acceso","Conteo de alertas de monitor previo","Etiqueta de malicia usada solo al evaluar","Identificador del episodio dentro del escenario","Intentos adicionales inyectados")),file.path("datos","diccionario.csv"),row.names=FALSE)
# ACF de los mismos residuos de entrenamiento utilizados en Ljung-Box.
# Exportamos valores calculados en R, sin reconstruir la ACF en el navegador.
residual_acf <- setNames(lapply(MODELS[1:3],function(m){
  r <- resid_primary[[paste0("forecast_",m)]]
  list(lag=1:48,acf=as.numeric(stats::acf(r,lag.max=48,plot=FALSE)$acf)[-1],
       band=1.96/sqrt(length(r)),n=length(r))
}),MODELS[1:3])
acf_table <- do.call(rbind,lapply(MODELS[1:3],function(m){
  a <- residual_acf[[m]]
  data.frame(seed=PRIMARY,model=m,lag=a$lag,acf=a$acf,band=a$band,n=a$n)
}))
csv(acf_table,"acf_residuos_pronostico.csv")
web <- c(summary,list(models=MODELS,scenarios=SCENARIOS,events=events,
  residual_acf=residual_acf,
  detection=detections,hourly=hourly[as.character(PRIMARY)],
  forecasts=forecasts_primary,anticipation=anticipation,
  raw=list(x=tail(bases[[as.character(PRIMARY)]]$end_min,5184)/60,
           y=tests[[as.character(PRIMARY)]]$Pico$activity)))
json(web,"datos_web.json")
writeLines(c("/* Datos calculados en presentacion.qmd */",paste0("window.CYBER_DATA=",jsonlite::toJSON(web,auto_unbox=TRUE,digits=8,na="null",null="null"),";")),file.path("resultados","datos_web.js"))

# 6. EXPORTACION DEL EJEMPLO DE ANTICIPACION.
anticipation_csv <- do.call(rbind,lapply(names(anticipation$models),function(m){
  z <- anticipation$models[[m]];ix <- match(z$x,anticipation$x)
  data.frame(model=m,origin=anticipation$origin,h=1:48,observed_normal=anticipation$normal[ix],
    observed_surprise=anticipation$attacked[ix],prediction=z$prediction,lower=z$lower,upper=z$upper)
}))
csv(anticipation_csv,"anticipacion.csv")

# 7. COMPROBACIONES INTERNAS RELEVANTES.
stopifnot(nrow(det)==3*5*4,all(det$tp+det$fp+det$fn+det$tn==18*96),all(fmetric$n==65))
for(i in seq_len(nrow(fmetric))){
  a <- fmetric[i,];z <- fore[fore$seed==a$seed&fore$model==a$model&fore$h==a$h&fore$set=="test"&fore$scenario==a$scenario,]
  stopifnot(abs(mean(abs(z$observed-z$prediction))-a$mae)<1e-10)
}
for(seed in SEEDS)for(m in MODELS){
  a <- det[det$seed==seed&det$model==m&det$scenario=="Pico",]
  b <- det[det$seed==seed&det$model==m&det$scenario=="Benigno",]
  stopifnot(a$tp+a$fp==b$fp)
}

# Resumen en consola al correr el script por separado.
print(forecast_mean)
print(subset(det,seed==PRIMARY&scenario=="Sostenido",select=c(model,tp,fn,recall,delay_median,fp)))

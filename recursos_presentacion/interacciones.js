(function(){
'use strict';
const D=window.CYBER_DATA,$=id=>document.getElementById(id),primary=String(D.meta.primary_seed);
const C={ARIMA:'#003d64',SARIMA:'#0897d5',ETS:'#e39918','Isolation Forest':'#7f9297','Ingenuo estacional':'#7f9297'},ink='#003d64',red='#e8442a',blue='#0897d5';
const fmt=(v,n=2)=>v==null?'No aplica':Number(v).toLocaleString('es-MX',{minimumFractionDigits:n,maximumFractionDigits:n});
const pct=v=>v==null?'No definida':fmt(v*100,1)+'%';
const time=h=>{const minutes=Math.round(h*60),day=Math.floor(minutes/1440),r=minutes-day*1440;return `Día ${day}, ${String(Math.floor(r/60)).padStart(2,'0')}:${String(r%60).padStart(2,'0')}`};
const events=(sc,seed=primary)=>D.events.filter(e=>String(e.seed)===String(seed)&&e.scenario===sc);
const ranges={},configs={},modes={},drawing=new Set(),updating=new Set();
const graphIds=['agregacion','deteccion','comparacion-detectores','sostenido','malicia','pronostico','mae','diagnosticos','anticipacion'];
function fill(id,values,label=x=>x){const e=$(id);e.replaceChildren();values.forEach(v=>e.add(new Option(label(v),v)));}
function line(x,y,name,color,extra={}){return {type:'scatter',mode:'lines',x,y,name,line:{color,width:1.3},customdata:x.map(time),hovertemplate:'%{customdata}<br>%{y:.2f}<extra>'+name+'</extra>',...extra};}
function axis(title,range){return {title:{text:title,font:{size:11},standoff:7},range,fixedrange:false,showline:true,mirror:true,linecolor:'#999',linewidth:.6,tickfont:{size:10},ticks:'outside',ticklen:3,gridcolor:'#eff1f2',zeroline:false,automargin:false};}
function base(id,range,height){return {width:$(id).clientWidth,height:height||parseInt($(id).style.height),autosize:true,paper_bgcolor:'#fff',plot_bgcolor:'#fff',font:{family:'LM, Arial',size:11,color:ink},margin:{l:57,r:14,t:36,b:39},legend:{orientation:'h',x:0,y:1.035,yanchor:'bottom',font:{size:10},traceorder:'normal'},xaxis:axis('Hora simulada',range),yaxis:axis('Intentos / 15 min'),hovermode:'closest',dragmode:modes[id]||'zoom',uirevision:id};}
function axisKeys(layout,kind='xy'){return Object.keys(layout).filter(k=>new RegExp('^['+kind+']axis\\d*$').test(k));}
function rangeFrom(update,key){return update[key+'.range']||(update[key+'.range[0]']!=null?[update[key+'.range[0]'],update[key+'.range[1]']]:null);}
function dayTicks(range){
 const span=range[1]-range[0],step=[.25,.5,1,3,6,12,24,48,96,168].find(s=>span/s<=7)||168,vals=[];
 for(let h=Math.ceil(range[0]/step)*step;h<=range[1]+1e-8;h+=step)vals.push(h);
 return {tickvals:vals,ticktext:vals.map(h=>span>72?fmt(h/24,0):fmt(h/24,2))};
}
function isDayAxis(id,key){return configs[id]?.dayAxes.includes(key);}
function clearY(id){if(ranges[id])for(const k of Object.keys(ranges[id]))if(k.startsWith('y'))delete ranges[id][k];}
async function rememberView(id,u){
 if(drawing.has(id)||updating.has(id))return;
 const el=$(id),changes={};ranges[id]=ranges[id]||{};
 for(const k of axisKeys(el.layout)){
  if(u[k+'.autorange'])delete ranges[id][k];
  const r=rangeFrom(u,k);if(!r)continue;
  ranges[id][k]=r.slice();
  if(k.startsWith('x')){
   // All temporal panels in one figure share the same visible interval.
   for(const xk of axisKeys(el.layout,'x')){
    ranges[id][xk]=r.slice();
    const actual=el._fullLayout[xk].range;
    if(Math.abs(actual[0]-r[0])>1e-8||Math.abs(actual[1]-r[1])>1e-8)changes[xk+'.range']=r;
    if(isDayAxis(id,xk))for(const [name,value]of Object.entries(dayTicks(r)))changes[xk+'.'+name]=value;
   }
  }
 }
 if(Object.keys(changes).length){updating.add(id);try{await Plotly.relayout(el,changes);}finally{updating.delete(id);}}
}
async function draw(id,traces,range,extra={}){
 const layout={...base(id,range),...extra},old=configs[id];
 for(const k of axisKeys(layout)){
  layout[k]={...layout[k],fixedrange:false};
  // Keep a vertical zoom only while the axis represents the same quantity.
  if(old&&k.startsWith('y')&&old.axisUnits[k]!==layout[k].title?.text&&ranges[id])delete ranges[id][k];
 }
 const axisUnits=Object.fromEntries(axisKeys(layout).map(k=>[k,layout[k].title?.text]));
 const dayAxes=axisKeys(layout,'x').filter(k=>layout[k].title?.text==='Día simulado'||layout[k]._dayAxis);
 dayAxes.forEach(k=>delete layout[k]._dayAxis);
 configs[id]={traces,range:range.slice(),extra,dayAxes,axisUnits};
 if(ranges[id])for(const [k,v]of Object.entries(ranges[id]))if(layout[k]){
  layout[k]={...layout[k],range:v,autorange:false};
  if(dayAxes.includes(k))Object.assign(layout[k],dayTicks(v));
 }
 const el=$(id);drawing.add(id);
 try{await Plotly.react(el,traces,layout,{responsive:true,displaylogo:false,scrollZoom:true,displayModeBar:false,doubleClick:false});}
 finally{drawing.delete(id);}
 if(!el._cyberListener){el._cyberListener=true;el.on('plotly_relayout',u=>rememberView(id,u));
  el.on('plotly_doubleclick',()=>{resetView(id);return false;});}
}
function title(text,x,y){return {xref:'paper',yref:'paper',x,y,text:'<b>'+text+'</b>',showarrow:false,xanchor:'center',yanchor:'bottom',font:{family:'LM, Arial',size:12,color:ink}};}
async function resetView(id,all=false){
 const el=$(id),cfg=configs[id];if(!cfg)return;
 delete ranges[id];let r=cfg.range.slice();
 if(all){
  const xs=cfg.traces.flatMap(t=>(t.x||[]).filter(Number.isFinite));
  if(xs.length)r=[Math.min(...xs),Math.max(...xs)];
  if(id==='comparacion-detectores')r=[-.6,2.6];
 }
 const update={};
 for(const k of axisKeys(el.layout)){
  if(k.startsWith('x')){update[k+'.range']=r;update[k+'.autorange']=false;
   if(isDayAxis(id,k))for(const [name,value]of Object.entries(dayTicks(r)))update[k+'.'+name]=value;
  }else if(cfg.extra[k]?.range&&!all){update[k+'.range']=cfg.extra[k].range.slice();update[k+'.autorange']=false;}
  else{update[k+'.autorange']=true;}
 }
 await Plotly.relayout(el,update);
}
async function toolAction(id,action){
 const el=$(id);if(!el._fullLayout)return;
 if(action==='zoom'||action==='pan'){
  modes[id]=action;
  document.querySelectorAll('[data-for="'+id+'"] [data-action="zoom"], [data-for="'+id+'"] [data-action="pan"]').forEach(b=>{const on=b.dataset.action===action;b.classList.toggle('active',on);b.setAttribute('aria-pressed',String(on));});
  return Plotly.relayout(el,{dragmode:action});
 }
 if(action==='reset'||action==='all')return resetView(id,action==='all');
 const update={};
 for(const k of axisKeys(el.layout)){
  const [a,b]=el._fullLayout[k].range,span=b-a,mid=(a+b)/2;
  if(action==='in'||action==='out'){const half=span*(action==='in'?.35:.7);update[k+'.range']=[mid-half,mid+half];}
  if(k.startsWith('x')&&(action==='back'||action==='forward')){const delta=span*(action==='back'?-.4:.4);update[k+'.range']=[a+delta,b+delta];}
 }
 return Plotly.relayout(el,update);
}
function installTools(){
 const buttons=[['zoom','Zoom','Seleccionar un rectángulo para ampliar los ejes X e Y'],['pan','Desplazar','Arrastrar para desplazar ambos ejes'],['back','←','Desplazar la vista a la izquierda'],['forward','→','Desplazar la vista a la derecha'],['in','+','Acercar ambos ejes'],['out','−','Alejar ambos ejes'],['all','Todo','Ver todos los datos'],['reset','Restablecer','Volver a la vista inicial y restablecer la escala vertical']];
 for(const id of graphIds){
  const el=$(id),visual=el.closest('.visual');let bar=visual.querySelector('.controlbar');
  if(!bar){bar=document.createElement('div');bar.className='controlbar';visual.prepend(bar);}
  const holder=document.createElement('div');holder.className='chart-tools';holder.dataset.for=id;holder.setAttribute('role','group');holder.setAttribute('aria-label','Explorar la gráfica');
  for(const [action,label,description]of buttons){const b=document.createElement('button');b.type='button';b.dataset.action=action;b.textContent=label;b.title=description;b.setAttribute('aria-label',description);if(action==='zoom'||action==='pan'){b.classList.toggle('active',action==='zoom');b.setAttribute('aria-pressed',String(action==='zoom'));}holder.append(b);}
  holder.addEventListener('click',e=>{const a=e.target.closest('button')?.dataset.action;if(a)toolAction(id,a);});bar.append(holder);
 }
}
function shade(ev,ref='x',domain=[0,1]){return ev.map(e=>({type:'rect',xref:ref,yref:'paper',x0:e.start_min/60,x1:e.end_min/60,y0:domain[0],y1:domain[1],fillcolor:'rgba(232,68,42,.12)',line:{width:0},layer:'below'}));}
function dayAxis(range){return {...axis('Día simulado',range),...dayTicks(range),_dayAxis:true};}
function alarms(z,md,xa='x',ya='y'){
 return [true,false].map(hit=>{const ix=md.alarm.flatMap((v,i)=>v&&Boolean(z.attack[i])===hit?[i]:[]);return {type:'scatter',mode:'markers',x:ix.map(i=>z.x[i]),y:ix.map(i=>z.y[i]),xaxis:xa,yaxis:ya,name:hit?'Aviso en ataque':'Falso aviso',marker:{color:hit?red:'#7f9297',symbol:hit?'triangle-up':'circle-open',size:5},customdata:ix.map(i=>time(z.x[i])),hovertemplate:'%{customdata}<br>%{y} intentos / 15 min<extra>'+(hit?'Intervalo malicioso señalado':'Aviso fuera de ataque')+'</extra>'};});
}
function gran(){
 const minutes=+$('gran-window').value,r=D.raw,x=[],y=[],step=minutes/5;
 for(let i=0;i<r.y.length;i+=step){const last=Math.min(i+step,r.y.length);x.push(r.x[last-1]);y.push(r.y.slice(i,last).reduce((a,b)=>a+b,0)/((last-i)*5));}
 const range=[103.5*24,105.1*24],ev=events('Pico');
 const top=line(r.x,r.y.map(v=>v/5),'Ventana de 5 minutos',blue);
 const bottom=line(x,y,`Ventana de ${minutes} minutos`,blue,{xaxis:'x2',yaxis:'y2'});
 return draw('agregacion',[top,bottom],range,{showlegend:false,margin:{l:60,r:13,t:35,b:35},xaxis:{...dayAxis(range),domain:[0,1],anchor:'y',title:{text:''}},yaxis:{...axis('Intentos por minuto'),domain:[.62,1]},xaxis2:{...dayAxis(range),anchor:'y2',matches:'x',title:{text:'Día simulado',font:{size:10},standoff:4}},yaxis2:{...axis('Intentos por minuto'),domain:[0,.38]},shapes:[...shade(ev,'x',[.62,1]),...shade(ev,'x2',[0,.38])],annotations:[title('Ventana de 5 minutos',.5,1.035),title(`Ventana de ${minutes} minutos`,.5,.415)]});
}
function detection(){
 const seed=$('det-seed').value,sc=$('det-scenario').value,m=$('det-model').value;
 $('det-model').parentElement.hidden=sc==='Todos';
 const range=[2448,2880];
 if(sc==='Todos'){
  const traces=[],extra={height:330,showlegend:false,margin:{l:65,r:15,t:34,b:34},annotations:[],shapes:[]};
  ['Pico','Sostenido','Rafagas'].forEach((s,i)=>{
   const z=D.detection[seed][s],a=i?'x'+(i+1):'x',b=i?'y'+(i+1):'y',domain=[[.80,1],[.40,.60],[0,.20]][i];
   traces.push(line(z.x,z.y,s,ink,{xaxis:a,yaxis:b,line:{color:ink,width:1}}));
   extra['xaxis'+(i?i+1:'')]={...dayAxis(range),anchor:b,title:{text:i===2?'Día simulado':'',font:{size:10},standoff:4},...(i?{matches:'x'}:{})};
   extra['yaxis'+(i?i+1:'')]={...axis('Intentos / 15 min'),domain};
   extra.shapes.push(...shade(events(s,seed),a,domain));
   extra.annotations.push(title({Pico:'Pico',Sostenido:'Sostenido',Rafagas:'Ráfagas'}[s],.5,domain[1]+.035));
  });
  $('det-metrics').textContent='';return draw('deteccion',traces,range,extra);
 }
 const z=D.detection[seed][sc],md=z.models[m],tr=[line(z.x,z.y,'Actividad observada',ink)],extra={height:330,shapes:shade(events(sc,seed)),xaxis:dayAxis(range)};
 if(m==='Isolation Forest'){
  extra.yaxis={...axis('Intentos / 15 min'),domain:[.55,1]};extra.xaxis={...dayAxis(range),showticklabels:false,title:{text:''}};
  extra.xaxis2={...dayAxis(range),anchor:'y2',matches:'x'};extra.yaxis2={...axis('Puntuación IF'),domain:[0,.29]};
  extra.annotations=[title('Puntuación de anomalía',.5,.335)];
  extra.shapes=[...shade(events(sc,seed),'x',[.55,1]),...shade(events(sc,seed),'x2',[0,.29])];
  tr.push(line(z.x,md.score,'Puntuación de anomalía',C[m],{xaxis:'x2',yaxis:'y2'}),line([z.x[0],z.x.at(-1)],[md.threshold,md.threshold],'Umbral 99%',red,{xaxis:'x2',yaxis:'y2',line:{color:red,width:1,dash:'dot'}}));
 }else tr.push(line(z.x,md.expected,'Valor esperado '+m,C[m],{line:{color:C[m],width:1.6,dash:'dash'}}));
 tr.push(...alarms(z,md));
 const v=md.metrics;
 $('det-metrics').textContent=`18 días completos · Sensibilidad ${pct(v.recall)} · Falsos avisos ${v.fp} · Demora mediana ${v.delay_median==null?'No aplica':fmt(v.delay_median,0)+' min'}`;
 return draw('deteccion',tr,range,extra);
}
function sustained(){
 const choice=$('sust-model').value,z=D.detection[primary].Sostenido,range=[109.5*24,113.5*24],ev=events('Sostenido');
 const models=choice==='Todos'?D.models:[choice],tr=[],extra={height:320,margin:{l:48,r:12,t:34,b:34},showlegend:false,annotations:[],shapes:[]};
 models.forEach((m,i)=>{
  const md=z.models[m],suffix=i?i+1:'',xa='x'+suffix,ya='y'+suffix,cols=models.length>1;
  const xd=cols?(i%2?[.56,1]:[0,.44]):[0,1],yd=cols?(i<2?[.64,1]:[0,.36]):[0,1];
  const score=m==='Isolation Forest',y=score?md.score:z.y;
  tr.push(line(z.x,y,score?'Puntuación IF':'Observado',ink,{xaxis:xa,yaxis:ya,line:{color:ink,width:.8}}));
  if(score)tr.push(line([z.x[0],z.x.at(-1)],[md.threshold,md.threshold],'Umbral 99%',C[m],{xaxis:xa,yaxis:ya,line:{color:C[m],dash:'dash',width:1.5}}));
  else tr.push(line(z.x,md.expected,'Esperado '+m,C[m],{xaxis:xa,yaxis:ya,line:{color:C[m],width:1.4}}));
  const hits=md.alarm.flatMap((v,j)=>v?[j]:[]);
  tr.push({type:'scatter',mode:'markers',x:hits.map(j=>z.x[j]),y:hits.map(j=>y[j]),xaxis:xa,yaxis:ya,name:'Aviso '+m,marker:{color:red,size:3},customdata:hits.map(j=>time(z.x[j])),hovertemplate:'%{customdata}<br>%{y:.3f}<extra>Aviso '+m+'</extra>'});
  extra['xaxis'+suffix]={...dayAxis(range),domain:xd,anchor:ya,title:{text:cols&&i<2?'':'Día simulado',font:{size:10},standoff:4},...(i?{matches:'x'}:{})};
  extra['yaxis'+suffix]={...axis(score?'Puntuación':'Intentos / 15 min'),domain:yd,anchor:xa};
  extra.shapes.push(...shade(ev,xa,yd));
  extra.annotations.push(title(m,(xd[0]+xd[1])/2,yd[1]+.035));
 });
 $('sust-metrics').textContent=choice==='Todos'?'':`${choice} · Episodio completo: ${z.models[choice].metrics.tp} de 920 intervalos maliciosos señalados`;
 return draw('sostenido',tr,range,extra);
}
function context(){
 const sc=$('context').value,z=D.detection[primary][sc],md=z.models.ETS,e=events(sc)[0],hits=md.alarm.flatMap((a,i)=>a?[i]:[]);
 const tr=[line(z.x,z.y,'Actividad idéntica en ambos contextos',ink),line(z.x,md.expected,'Esperado por ETS',C.ETS,{line:{color:C.ETS,width:1.5,dash:'dash'}}),{type:'scatter',mode:'markers',x:hits.map(i=>z.x[i]),y:hits.map(i=>z.y[i]),name:'Mismos avisos',marker:{color:red,symbol:'diamond',size:6},customdata:hits.map(i=>time(z.x[i])),hovertemplate:'%{customdata}<br>%{y} intentos / 15 min<extra>Mismo aviso estadístico</extra>'}];
 const count=hits.filter(i=>z.x[i]*60>e.start_min&&z.x[i]*60-15<e.end_min).length;
 $('context-result').textContent=sc==='Pico'?`Pico malicioso: ${count} avisos durante el primer episodio.`:`Prueba autorizada: los mismos ${count} avisos son falsos positivos al evaluar ataques.`;
 return draw('malicia',tr,[2504,2508],{margin:{l:65,r:16,t:32,b:37},shapes:[{type:'line',xref:'x',yref:'paper',x0:e.start_min/60,x1:e.start_min/60,y0:0,y1:1,line:{color:red,width:1,dash:'dot'}}]});
}
function forecast(){
 const m=$('fore-model').value,h=+$('fore-h').value,o=+$('fore-origin').value,hr=D.hourly[primary],f=D.forecasts[m][o],x=f.x.slice(0,h),naive=D.forecasts['Ingenuo estacional'][o];
 const color=m==='ETS'?blue:C[m];
 const tr=[line(hr.x,hr.y,'Observado',ink),{type:'scatter',mode:'lines',x,y:f.lower.slice(0,h),line:{width:0},showlegend:false,hoverinfo:'skip'},{type:'scatter',mode:'lines',x,y:f.upper.slice(0,h),line:{width:0},fill:'tonexty',fillcolor:'rgba(8,151,213,.19)',name:'Banda marginal 90%',hoverinfo:'skip'},line(x,f.prediction.slice(0,h),m,color,{mode:h===1?'markers':'lines',line:{color,width:1.9},marker:{size:7}})];
 if(m!=='Ingenuo estacional')tr.push(line(x,naive.prediction.slice(0,h),'Ingenuo estacional',C['Ingenuo estacional'],{line:{color:C['Ingenuo estacional'],width:1,dash:'dot'}}));
 const v=D.forecast_metrics.find(r=>String(r.seed)===primary&&r.scenario==='Normal'&&r.model===m&&r.h===h);
 $('fore-metrics').textContent=`${m}, hora t+${h} · MAE ${fmt(v.mae)} alertas/h · Evaluación en ${v.n} orígenes de prueba de la repetición principal`;
 return draw('pronostico',tr,[o-24,o+h],{yaxis:axis('Alertas del monitor / hora'),shapes:[{type:'line',xref:'x',yref:'paper',x0:o,x1:o,y0:0,y1:1,line:{color:red,width:1,dash:'dot'}}]});
}
function mae(){
 const tr=['ARIMA','SARIMA','ETS','Ingenuo estacional'].map(m=>{const rows=D.forecast_mean.filter(r=>r.model===m).sort((a,b)=>a.h-b.h);return {type:'scatter',mode:'lines+markers',name:m,x:rows.map(r=>r.h),y:rows.map(r=>r.mae),line:{color:C[m],width:1.5},marker:{size:4},error_y:{type:'data',symmetric:false,array:rows.map(r=>r.mae_max-r.mae),arrayminus:rows.map(r=>r.mae-r.mae_min),color:C[m],thickness:.7,width:2},customdata:rows.map(r=>[r.mae_min,r.mae_max]),hovertemplate:m+'<br>Horizonte: %{x} h<br>MAE: %{y:.3f} alertas/h<br>Mín./máx. entre semillas: %{customdata[0]:.3f} / %{customdata[1]:.3f}<extra></extra>'};});
 const chart=draw('mae',tr,[0,50],{height:270,xaxis:{...axis('Horizonte (horas)',[0,50]),tickvals:[1,6,24,48]},yaxis:{...axis('MAE (alertas / hora)'),range:[0,6]},legend:{x:.69,y:.03,font:{size:10},orientation:'v'}});
 $('mae-result').textContent='';
 return chart;
}
function compareDetectors(){
 const scenarios=['Pico','Sostenido','Rafagas'];
 const tr=D.models.map(m=>{const values=scenarios.map(sc=>D.detection_mean.find(r=>r.scenario===sc&&r.model===m).recall*100);return {type:'bar',name:m,x:[0,1,2],y:values,marker:{color:C[m]},text:values.map(v=>fmt(v,1)),textposition:'outside',textfont:{size:10,color:ink},cliponaxis:false,hovertemplate:m+'<br>%{customdata}<br>Intervalos maliciosos señalados: %{y:.2f}%<extra>Promedio de tres repeticiones</extra>',customdata:['Pulso breve','Aumento sostenido','Ráfagas periódicas']};});
 return draw('comparacion-detectores',tr,[-.6,2.6],{height:275,margin:{l:67,r:16,t:38,b:35},barmode:'group',bargap:.25,xaxis:{...axis('',[-.6,2.6]),tickvals:[0,1,2],ticktext:['Breve','Persistente','Periódico'],showgrid:false},yaxis:{...axis('Intervalos maliciosos señalados (%)',[0,110]),dtick:20},legend:{orientation:'h',x:.5,xanchor:'center',y:1.035,yanchor:'bottom',font:{size:10}}});
}
function diagnostics(){
 const tr=[],extra={height:210,margin:{l:42,r:12,t:28,b:34},showlegend:false,shapes:[],annotations:[]};
 const aa=Object.values(D.residual_acf),yr=[Math.min(...aa.flatMap(a=>a.acf),...aa.map(a=>-a.band),0)*1.15,Math.max(...aa.flatMap(a=>a.acf),...aa.map(a=>a.band),0)*1.15];
 ['ARIMA','SARIMA','ETS'].forEach((m,i)=>{
  const a=D.residual_acf[m],suffix=i?i+1:'',xa='x'+suffix,ya='y'+suffix,domain=[[0,.285],[.355,.64],[.71,1]][i],xx=[],yy=[];
  a.lag.forEach((lag,j)=>{xx.push(lag,lag,null);yy.push(0,a.acf[j],null);});
  tr.push({type:'scatter',mode:'lines',x:xx,y:yy,xaxis:xa,yaxis:ya,name:'Residuos '+m,line:{color:C[m],width:1},hoverinfo:'skip'});
  tr.push({type:'scatter',mode:'markers',x:a.lag,y:a.acf,xaxis:xa,yaxis:ya,name:'Residuos '+m,marker:{color:C[m],size:3},customdata:a.lag.map(()=>a.band),hovertemplate:'<b>Residuos '+m+'</b><br>Rezago: %{x} horas<br>ACF: %{y:.4f}<br>Referencia aproximada: ±%{customdata:.4f}<extra>Residuos de entrenamiento</extra>'});
  extra['xaxis'+suffix]={...axis('Rezago (horas)',[0,49]),domain,anchor:ya,tickvals:[0,12,24,36,48],...(i?{matches:'x'}:{})};
  extra['yaxis'+suffix]={...axis('ACF',yr),anchor:xa,...(i?{matches:'y'}:{})};
  for(const b of [-a.band,0,a.band])extra.shapes.push({type:'line',xref:xa,yref:ya,x0:0,x1:49,y0:b,y1:b,line:{color:'#7f9297',width:b===0?.6:.8,dash:b===0?'solid':'dash'},layer:'below'});
  extra.annotations.push(title('Residuos '+m,(domain[0]+domain[1])/2,1.035));
 });
 return draw('diagnosticos',tr,[0,49],extra);
}
function anticipation(){
 const a=D.anticipation,z=a.models.ETS;
 const tr=[{type:'scatter',mode:'lines',x:z.x,y:z.lower,line:{width:0},showlegend:false,hoverinfo:'skip'},{type:'scatter',mode:'lines',x:z.x,y:z.upper,line:{width:0},fill:'tonexty',fillcolor:'rgba(8,151,213,.16)',name:'Banda marginal 90%',hoverinfo:'skip'},line(a.x,a.normal,'Futuro normal','#7f9297'),line(a.x,a.attacked,'Futuro con pulso',red,{line:{color:red,width:1.8}}),line(z.x,z.prediction,'Pronóstico ETS emitido antes',blue,{line:{color:blue,width:1.7,dash:'dash'}})];
 const visible=[];a.x.forEach((x,i)=>{if(x>=a.origin-8&&x<=a.origin+10)visible.push(a.normal[i],a.attacked[i]);});visible.push(...z.upper.slice(0,10),...z.lower.slice(0,10));
 const low=Math.min(...visible),high=Math.max(...visible),pad=(high-low)*.07;
 return draw('anticipacion',tr,[a.origin-8,a.origin+10],{height:235,margin:{l:65,r:17,t:36,b:34},yaxis:axis('Alertas del monitor / hora',[Math.max(0,low-pad),high+pad]),legend:{orientation:'h',x:0,y:1.035,yanchor:'bottom',font:{size:9.5}},shapes:[{type:'line',xref:'x',yref:'paper',x0:a.origin,x1:a.origin,y0:0,y1:1,line:{color:ink,width:1,dash:'dot'}}]});
}
function init(){
 fill('det-scenario',['Todos',...D.scenarios],s=>({Todos:'Tres episodios',Normal:'Normal',Pico:'Pico',Sostenido:'Sostenido',Rafagas:'Ráfagas',Benigno:'Benigno'}[s]));
 fill('det-model',D.models);fill('det-seed',Object.keys(D.detection),s=>'Repetición '+(Object.keys(D.detection).indexOf(s)+1));
 fill('sust-model',['Todos',...D.models],s=>s==='Todos'?'Cuatro modelos':s);
 fill('fore-model',Object.keys(D.forecasts));fill('fore-origin',Object.keys(D.forecasts.ETS),s=>time(+s));
 $('det-scenario').value='Todos';$('det-model').value='ETS';$('det-seed').value=primary;$('sust-model').value='Todos';$('fore-model').value='ETS';$('fore-origin').value='2448';$('fore-h').value='48';
 installTools();
 for(const id of ['det-scenario','det-model','det-seed'])$(id).addEventListener('change',()=>{if(id!=='det-model')clearY('deteccion');detection();});
 $('gran-window').onchange=gran;$('sust-model').onchange=sustained;$('context').onchange=context;
 for(const id of ['fore-model','fore-h','fore-origin'])$(id).addEventListener('change',()=>{if(id==='fore-origin')delete ranges.pronostico;forecast();});
 window.CYBER_READY=Promise.all([gran(),detection(),compareDetectors(),sustained(),context(),forecast(),mae(),diagnostics(),anticipation()]);
 const resize=()=>{document.querySelectorAll('.present .interactive-plot').forEach(el=>{if(el._fullLayout)Plotly.Plots.resize(el);});};
 Reveal.on('slidechanged',()=>requestAnimationFrame(resize));window.addEventListener('resize',resize);
 document.querySelectorAll('select,.chart-tools button').forEach(el=>el.addEventListener('keydown',e=>e.stopPropagation()));
}
if(window.Reveal&&Reveal.isReady())init();else if(window.Reveal)Reveal.on('ready',init);else document.addEventListener('DOMContentLoaded',init);
})();

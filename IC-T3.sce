//***Comentários***

//Se for desejado um número específico (num) de gerações, basta digitar no console: [x,y,f]=evolution(num)

//f é o resultado de eggholder(x,y)

//Obtive ótimos resultados com 150 gerações. Para tal quantidade, é gasto um pouco mais de 2 minutos em meu notebook para fornecer os resultados

//Nesse caso, digite no console o seguinte comando: [x,y,f]=evolution(150)

//Os melhores resultados foram para aproximadamente x=522 e y=413

//Ao final é plotado um gráfico da função eggholder 

function u=eggholder(x, y) //aplica a função eggholder para x e y
u = -(y+47)*sin(sqrt(abs(x/2+y+47))) - x*sin(sqrt(abs(x-(y+47))))
endfunction



function [d]=binar(b) //transforma um vetor binário no seu correspondente em decimal
    d=0
    len = length(b)
    for i=1:len
        d=d+2^(len-i)*b(i)
    end
endfunction



function num=modify(b) //num é o correspondente a b no intervalo [400,600]
    num = 400+200*b/(2^20-1)
endfunction



function vet=g1()  //gera aleatoriamente 0 ou 1

  
        u = prbs_a(2,1)(1)
       
            if u==-1 then
               u=0
            end
        
        vet=u 
    

endfunction



function pop=ini_pop()  //gera a população inicial

    pop=[]
    for i=1:100
        
        u = prbs_a(40,7)
        for j=1:40
            if u(j)==-1 then
               u(j)=0
            end
        end  
        pop=[pop;u]     
    end

endfunction



function egg= fun(pop) //aplica a função eggholder a cada x e y das linhas de pop
    egg = []
    tam = size(pop)(1)
    for i=1:tam
        aux=pop(i,1:20)
        x = modify(binar(aux))
        auy=pop(i,21:40)
        y = modify(binar(auy))
        res = eggholder(x,y)
        egg=[egg;res]
        
    end
endfunction





function fit=fitness(egg)  //função de avaliação
 
    [maior,ind] = max(egg)
    
    for i=1:length(egg)
        if egg(i)<0 then
            fit(i)=abs(egg(i))+maior //indivíduos com valores negativos na função eggholder devem ser valorizados na nota de avaliação
        else 
            fit(i)=egg(i)*0.7  //indivíduos com valores positivos na função eggholder devem ser penalizados na nota de avaliação
           end
    end
    
endfunction




function p=percentages(fit) //calcula a porcentagem de cada nota em relação a soma de todas
    for i=1:length(fit)
        p(i)=fit(i)*100/sum(fit)
    end
endfunction



function r=acc_pb(p)   //porcentagens acumuladas
    r(1)=p(1)
    for i=2:length(p)
        r(i)=r(i-1)+p(i)
    end
endfunction



function prep=roulette(r)  //roleta: r é um vetor com os intervalos de porcentagem configurados

prep=[]

while length(prep) <length(r)


    y = grand(1, "uin", 0, 100) //valores aleatórios y serão gerados
    for k=1:length(r)  //verifica-se a qual intervalo o y pertece e, assim, o índice desse intervalo vai para prep
        if y<=r(k) then   
            break
        end
    end
    
prep=[prep k]    
end
    
endfunction






function cross_seq=selection(pop,fit,pop_size) //seleção de uma sequência para cruzamento. Nela, cada dois indivíduos próximos cruzarão.
    cross_seq=[]
    p=percentages(fit)
    r=acc_pb(p)
    indices=roulette(r) //utilização da roleta
    
    for i=1:pop_size
        cross_seq(i,:)=pop(indices(i),:)
        
    end
    
endfunction






function filhos=crossover(pais)  //pais(1) cruza com pais(2), pais(3) cruza com pais(4) e assim por diante: pais(i) cruza com pais(i+1)
    filhos=[]
    i=1
//aleatoriamente são selecionados dois pontos de corte para a parte x e dois pontos para a parte y de pai(i) e de pai(i+1)
while i<size(pais)(1)
    cpx1 = grand(1, "uin",2 ,10 )
    cpx2 = grand(1, "uin",11,19 )
    while cpx1==cpx2
        cpx1 = grand(1, "uin",2 ,10)
        cpx2 = grand(1, "uin",11,19 )  
    end
    cpy1 = grand(1, "uin",22 ,30)
    cpy2 = grand(1, "uin",31 ,39)
    while cpy1==cpy2
        cpy1 = grand(1, "uin",22 ,30) 
        cpy2 = grand(1, "uin",31,39)
    end
    //troca de segmentos de bits (definidos pelos pontos de corte) entre pais(i) e pais(i+1)
    filhos=[filhos;[pais(i+1,1:cpx1) pais(i,cpx1+1:cpx2) pais(i+1,cpx2+1:20) pais(i,21:cpy1) pais(i+1,cpy1+1:cpy2) pais(i,cpy2+1:40)]]
    filhos=[filhos;[pais(i,1:cpx1) pais(i+1,cpx1+1:cpx2) pais(i,cpx2+1:20) pais(i+1,21:cpy1) pais(i,cpy1+1:cpy2) pais(i+1,cpy2+1:40)]]
    i=i+2
end
endfunction






function mutantes=mutation(pop) //aplicar mutação em alguns indivíduos da população
    mutantes=pop
    
    [v,j]= max(fitness(fun(pop)))
    alt=rand()  
   
    qtd=(size(pop)(1))*alt/5
        for i=1:round(qtd)
            sel=grand(1, "uin",1 , size(pop)(1))
            while sel==j //não modificar o indivíduo com melhor avaliação
                sel=grand(1, "uin",1 , size(pop)(1))
                continue
            end
            
            //os bits 1,20,21 e 40 (primeiro e último bit de x, primeiro e último bit de y) da linha self serão mudados de forma aleatória
            mutantes(sel,1)= g1()
            mutantes(sel,20)= g1()          
            mutantes(sel,21)= g1()
            mutantes(sel,40)= g1()  
            
        end
endfunction





function new_g=survival_game(pais,filhos) //pais disputando com filhos pela sobrevivência
    
    fit_pais=fitness(fun(pais))
    fit_filhos=fitness(fun(filhos))
    
    new_g=[]    //nova geração
    t=length(fit_pais)
    m=1
    
    while m<round(t*0.05) //t*0.05 corresponde a 5% de t
    [v,i]=max(fit_pais)
    new_g(m,:)= pais(i,:) //nova geração recebe o indivíduo com maior nota entre os pais
    fit_pais(i)=[] //retirar nota do indivíduo
    pais(i,:)=[]  //retirar indivíduo 
    [h,j]=max(fit_filhos)
   
    while v==h // se a maior nota entre os filhos for igual a nota que foi colocada anteriormente em new_g, então fazer substituição em filhos por uma nota aleatória dos pais
        r=grand(1, "uin",1,size(pais)(1))
        fit_filhos(j)=fit_pais(r)
        filhos(j,:)=pais(r,:)
        fit_pais(r)=[] //retirar nota do indivíduo
        pais(r,:)=[]  //retirar indivíduo 
        [h,j]=max(fit_filhos)
        
    end
    new_g(m+1,:)=filhos(j,:)  //nova geração recebe o indivíduo com maior nota entre os filhos
    fit_filhos(j)=[] //retirar nota do indivíduo
    filhos(j,:)=[]    //retirar indivíduo 
    
    m=m+2
    end
    
    
    
    while size(new_g)(1)<t   // usar indivíduos aleatórios entre pais e filhos para popular o que falta da nova geração
        r1=grand(1, "uin",1,length(fit_filhos))
        r2=grand(1, "uin",1,length(fit_pais))
        new_g=[new_g; filhos(r1,:)]
        new_g=[new_g; pais(r2,:)]
        fit_filhos(r1)=[]
        filhos(r1,:)=[]
        fit_pais(r2)=[]
        pais(r2,:)=[]
        end
  
    
endfunction






function [x,y,f]=evolution(num_g) //num_g é a quantidade desejada de gerações
   
    pop=ini_pop()
    sigma=[]
    sigma(1)=1
    for u=1:num_g
        tra=fun(pop)
        fit=fitness(tra)
        sigma(u+1)= stdev(fit)
        if sigma(u+1)<sigma(u)  then  //comparação do desvio padrão das notas de uma geração com a da anterior
          
            pop=mutation(pop) //aplicar mutação
            
          
        else 
            filhos=crossover(selection(pop,fit,length(fit))) //fazer cruzamento
            pop=survival_game(pop,filhos) //disputa entre pais e filhos pela sobrevivência
           
         end
         
    end
    
    [row,i]=max(fitness(fun(pop)))
    x=modify(binar(pop(i,1:20)))   //obtenção de x em decimal para o intervalo [400,600]
    y=modify(binar(pop(i,21:40))) //obtenção de y em decimal para o intervalo [400,600]
    f=eggholder(x,y)
endfunction
    
  




//Plota o gráfico de eggholder para x e y no intervalo [400,600]
u = linspace(400,600,50);
v = linspace(400,600,50); 

z = feval(u,v,eggholder)';

scf()
surf(u,v,z)

gcf().color_map = jetcolormap(100);
set(gcf(), "axes_size", [700 700], "rotation_style","multiple");
gca().rotation_angles = [75 -10];
    
    
    
    
    
    
    
    
    
    
    






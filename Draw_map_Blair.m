clear
close all
%addpath('c:\DFO-MPO\WangZ\BIO_computer\m_map')
DIR='./';
addpath('/media/data/wangz/m_map/colorbar')

%load data (annual / summmer)
load HadISST_annual.mat

R1='polygon_gomss_subarea_gom.csv';

R2='polygon_ss_merged.csv'

R3='polygon_gsl_subarea_gsl_clipped.csv';
R4='polygon_ns_subarea_sns.csv';

R5='polygon_ns_merged.csv'

R6='polygon_ls_merged.csv';

R7='polygon_hb_subarea_hb.csv';
R8='polygon_bb_subarea_bb.csv';
R9='polygon_bcs_subarea_bcs.csv';

R10='polygon_ca_subarea_sbs.csv';








T1='GoM';


T2='SS'

T3='GSL';
T4='SNS';


T5='NNS';


T6='LS'


T7='HB';
T8='BB';
T9='BCS';
T10='SBS';





% 
RI1=[0.4 0.6]
RI2=[0.6 0.8];
RI3=[0.8 1.0];
RI4=[1.0 1.2];
RI5=[1.2 1.4];
RI6=[1.4 1.6];
RI7=[1.6 1.8];
RI8=[1.8 2.0];
RI9=[2.0 2.2];
RI10=[2.2 2.4];
RI11=[2.4 2.6];
Lnum=11;
Lmin=0.4;Lmax=2.6



map=colormap(cmocean('thermal',Lnum+1))

axes('position',[0.90 0.3 0.02 0.65 ])
x=[0 1];
y=[Lmin:0.2:Lmax];
z=[y(:) y(:)];
pcolor(x,y,z);shading flat;
set(gca,'xticklabel','','fontsize',8)
caxis([Lmin Lmax]);
set(gca, 'YAxisLocation', 'right');

title('^oC')




axes('position',[0.05 0.3 0.80 0.65 ])


m_proj('lambert','long',[-150 -40],'lat',[40 80]);

m_grid('box','fancy','tickdir','in');

m_coast('patch',[0.7 0.7 0.7])
hold on
for n=1:10
subn=eval(['R',num2str(n)]);
filename=[DIR,subn];
data=load(filename);
lon=data(:,1);lat=data(:,2);
xlon=mean(lon(:));ylat=mean(lat(:));

if n==2  %SS
xlon=mean(lon(:))+2;ylat=mean(lat(:))-2;
end

if n==4  %SNS
xlon=mean(lon(:))+6;ylat=mean(lat(:))-2;
end

if n==5  %NNS
xlon=mean(lon(:))+4;ylat=mean(lat(:));
end

if n==6  %LS
xlon=mean(lon(:))+2;ylat=mean(lat(:));
end

if n==9   %BCS
xlon=mean(lon(:))-5;ylat=mean(lat(:))+1;
end
if n==3  %GSL
xlon=mean(lon(:))+0.8;ylat=mean(lat(:))+0.1;
end
if n==10   %BCS
xlon=-140;ylat=72;
end




if (n==1 || n==3 || n==8 || n==7)
 m_plot(xlon,ylat+0.1,'ow','Markersize',9)
 m_text(xlon,ylat+0.1,num2str(n),...
          'HorizontalAlignment','center', 'VerticalAlignment','middle', 'fontsize', 2,'color',[1 1 1])
else
 m_plot(xlon,ylat+0.1,'ok','Markersize',9)
 m_text(xlon,ylat+0.1,num2str(n),...
          'HorizontalAlignment','center', 'VerticalAlignment','middle', 'fontsize', 2,'color',[0 0 0])

end


for m=1:Lnum
    tmp=~isnan(discretize(Data_diff_rear(n),eval(['RI',num2str(m)])))
    if tmp
        m_patch(lon,lat,map(m,:));
    end
end
end

%Bravo
x=-51;y=56.5;
for m=1:Lnum
    tmp=~isnan(discretize(Br,eval(['RI',num2str(m)])))
    if tmp
m_plot(x,y,'s', 'markersize',10,'MarkerEdgeColor','k',...
                'MarkerFaceColor',map(m,:,:));
m_text(x+0.5, y-1. ,'\itBravo','fontsize',6,'color','k')
        
    end
end


%Papa
x=-144.8599;y=50.0378;
for m=1:Lnum
    tmp=~isnan(discretize(Pp,eval(['RI',num2str(m)])))
    if tmp
m_plot(x,y,'s', 'markersize',10,'MarkerEdgeColor','k',...
                'MarkerFaceColor',map(m,:,:));
m_text(x+1, y-1. ,'\itPapa','fontsize',6,'color','k')
        
    end
end







caxis([0.24 1.35])
axes('position',[0.1 0.05 0.80 0.17])


h=bar(Data_diff_rear,0.4,'FaceColor',[0 .5 .5],'EdgeColor',[0 .5 .5],'LineWidth',1);
hold on



% for annual
bar(6.4,Br,0.2,'FaceColor',[0.5 .5 .5],'EdgeColor',[0.5 .5 .5],'LineWidth',1);
text(6.3,0.6,'Bravo','fontsize',3,'color',[0 0 0]);
bar(9.4,Pp,0.15,'FaceColor',[0.5 .5 .5],'EdgeColor',[0.5 .5 .5],'LineWidth',1);
text(9.5,1.3,'Papa','fontsize',3,'color',[0 0 0]);
% 
%% for summer
% 
%  bar(6.4,Br,0.2,'FaceColor',[0.5 .5 .5],'EdgeColor',[0.5 .5 .5],'LineWidth',1);
%  text(6.3,1.3,'Bravo','fontsize',3,'color',[0 0 0]);
%  bar(9.4,Pp,0.15,'FaceColor',[0.5 .5 .5],'EdgeColor',[0.5 .5 .5],'LineWidth',1);
%  text(9.5,1.3,'Papa','fontsize',3,'color',[0 0 0]);

xticks([1:1:10]);
xticklabels({['1-',T1],['2-',T2],['3-',T3],['4-',T4],['5-',T5],['6-',T6],['7-',T7],['8-',T8],['9-',T9],['10-',T10]})

xlim([0.2 10.5])

ylabel('Te change(^oC)','fontsize',6)
grid on
ylim([0. 3])


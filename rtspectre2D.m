function [rspectr,tspectr,Ai,aispectr]=rtspectre2D(A,mi,ma,DIST,ANGL,mi2,ma2)
% Calcule les r-spectre,t-spectre et Ai-spectre de la surface A (2D)
%
% rspectr: r-spectre
% tspectr: theta-spectre
% Ai: indice d'anisotropie (colonne 1) et direction de celle-ci (colonne 2)
% en radians (0 degrees to the north (up) and angles increasing clockwise)
% aispectr: spectre radial d'anisotropie
% A: l'image carree a analyser
% mi: premier nbre d'onde utilise (facultatif)
% ma: dernier nbre d'onde utilise (facultatif)
% DIST: matrice des distances (facultatif)
% ANGL: matrice des angles (facultatif)
% mi2: premier nbre d'onde utilise pour calcul t-spectre et Ai (facultatif)
% ma2: dernier nbre d'onde utilise pour calcul t-spectre et Ai (facultatif)
%
% vincent Deblauwe, novembre 2007
%
if isvector(A),error('A n''est pas une matrice 2D. Utilisez rspectre1D() pour un signal 1D.'),end
% if size(A,1)~=size(A,2)
%     disp('L''image n''est pas carree! Application d''une corection...')
%     A=A-mean2(A);
%     if size(A,1)>size(A,2)
%         A=[A,zeros(size(A,1),size(A,1)-size(A,2))];
%     else
%         A=[A;zeros(size(A,2)-size(A,1),size(A,2))];
%     end
% end

[nlig,ncol]=size(A);
n0x=floor(ncol/2)+1;         % coordonnees du centre;
n0y=floor(nlig/2)+1;
%% Creation de la matrice des distances(R) et angles (theta)
if nargin>=5
    if size(DIST,1)~=nlig||size(DIST,2)~=ncol||size(ANGL,1)~=nlig||size(ANGL,2)~=ncol
        error('DIST et ANGL doivent avoir la meme taille que A')
    end
elseif nargin==4
    if size(DIST,1)~=nlig||size(DIST,2)~=ncol
        error('DIST doit avoir la meme taille que A')
    end
else
    f1=repmat(1:ncol,nlig,1)-n0x;   % dimentional coordinates
    f2=repmat((1:nlig)',1,ncol)-n0y;
    DIST=sqrt(f1.^2+f2.^2);
    if nargout>1
        ANGL=atan2(-f2,f1)*180/pi; % les angles repectent la convention trigonom�trique (0 � droite et augmente counterclock)
    end
end

%% Ini
if nargin==1
    mi=1;ma=min([n0y n0x]);
end

DISTMASK=DIST>=mi & DIST<=ma;          %le masque de l'interval de nombres d'onde que l'on prends en concid�ration
tmp=fft2(A);
tmp=fftshift(tmp);tmp(n0y,n0x)=0;
aspectr2D=(real(tmp).^2 + imag(tmp).^2)/(nlig*ncol)^4;    % le spectre d'amplitude 2D; syn (mais plus lent): aspectr2D=abs(tmp).^2;
% sig=std2(A).^2;                 % la variance de l'image qui est par definition �gale � la somme du p�riodo (1,1 except�)=sum(aspectr2D(:))
% aspectr2D=aspectr2D/sig;        % contribution relative a la variance. A ce stade: sum(sum(aspectr2D))=1
sig2=sum(aspectr2D(DISTMASK));    % 'variance' de l'interval pris en compte
aspectr2D=aspectr2D/sig2;         % contribution relative a la variance de l'intervale mi-ma (DIST).

%% Calcul du r-spectre et Ai-spectre
STEP=1;                                % le pas d'echantillonage de R (diam�tre des anneaux)
ray=(mi:STEP:ma);
rspectr=zeros(size(ray));
if nargout<4
    for i=1:size(ray,2)
        m=DIST>=ray(i)-STEP/2 & DIST<ray(i)+STEP/2;
        rspectr(i)=mean(aspectr2D(m)); % normalement on utilise la moyenne, mais pr�f�rer la somme pour donner plus de poid aux hautes fr�quences
    end
else % si le Ai-spectre est en nargout
    aispectr=zeros(size(ray));
    for i=1:size(ray,2)
        m=DIST>=ray(i)-STEP/2 & DIST<ray(i)+STEP/2;
        rspectr(i)=mean(aspectr2D(m));
        
        C = sum(aspectr2D(m)./sum(aspectr2D(m)).*(cos(ANGL(m)*2*pi/180)) );  % axe X (cos)
        S = sum(aspectr2D(m)./sum(aspectr2D(m)).*(sin(ANGL(m)*2*pi/180)) );  % axe Y (sin)
        aispectr(i)=sqrt( C^2 + S^2 );% Indice d'anisotropie pour l'anneau i
    end
end

%% Calcul du t-spectre
if nargout<=1,return,end
if nargin<=5, mi2=mi;ma2=ma;end
DISTMASK=DIST>=mi2 & DIST<=ma2;          %le masque de l'interval de nombres d'onde que l'on prends en concid�ration

%STEP=5;                                   % sortira 36 bins
STEP=5;                                % le pas d'�chantillonage de th�ta
anglbin=STEP:STEP:180;
tspectr=zeros(size(anglbin));
%pour chaque bin th�ta sauf la derni�re
for i=1:length(anglbin)-1;   
    m=find(DISTMASK & ANGL>=anglbin(i)-STEP & ANGL<anglbin(i));               % on cherche les pixels R et th�ta � retenir
    tspectr(1,i)=sum(aspectr2D(m))/size(m,1);   %effectue la division par le nombre de pixels dans bin(i) => � compenser par la suite si on veut le b-spectre cumul�
end

%pour la derni�re bin
m=find(DISTMASK & ANGL>=anglbin(length(anglbin))-STEP & ANGL<=anglbin(length(anglbin)));  % on cherche les pixels R et th�ta � retenir
tspectr(1,length(tspectr))=sum(aspectr2D(m))/size(m,1);  %effectue la division par le nombre de pixels dans bin(i) => � compenser par la suite si on veut le b-spectre cumul�

% Affichage sous forme de rose
% figure,rosevinc(STEP:STEP:360,[tspectr,tspectr],1);

%% Calcul de l'indice d'anisotropie
% S'assurer que aspectr2D est bien exprim� en proportion de la variance de
% l'interval de DIST [sum(aspectr2D(DISTMASK))==2] pour que Ai soit born�
% entre 0 et 1.
if nargout<=2,return,end

m=DISTMASK & ANGL>=0;                   % les quadrants � prendre en compte (NW & NE)
C = sum(aspectr2D(m)./sum(aspectr2D(m)).*(cos(ANGL(m)*2*pi/180)) );  % axe X (cos)
S = sum(aspectr2D(m)./sum(aspectr2D(m)).*(sin(ANGL(m)*2*pi/180)) );  % axe Y (sin)
% Indice d'anisotropie
Ai(1,1) = sqrt( C^2 + S^2 );
% Direction de l'anisotropie en radians
theta=atan2(S,C)/2;         % transform to 0-pi dirrection
Ai(1,2)=mod(-(theta-(pi/2)),pi); % put 0 degrees to the north (up) and angles increasing clockwise

% [theta,rbar]=circmeanw(ANGL(m)*2*pi/180,aspectr2D(m)./sum(aspectr2D(m)));
% Ai=rbar*sum(sum(m));   % circmeanw sort la r�sultante moyenne mais nous voulons la somme
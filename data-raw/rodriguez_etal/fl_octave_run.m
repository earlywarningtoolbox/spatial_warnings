// clear all
// close all

% Put where the data are
cd "/home/alex/work/2014-2015/SpatialStress/spatialwarnings"

% cd C:\angeles\Montpellier\Australia
%cd C:\sus\anna
pkg load image

%Put the image name
imfile = './tests/testthat/rodriguez2018/images/AZ-P5.jpg'
[~,imname,imext] = fileparts(imfile)
Image=imread(imfile);

tic

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Greythresh analysis
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

level = graythresh(Image)  % automatic optimal matlab threshold

% level=0.36   % put level for fixed threshold

bw = im2bw(Image,level) ;


% Calculate the cover
%--------------------------------------------------------------------------
bwbis = (bw == 0) ; % Make sure the vegetation sites are 1's
[nx, ny] = size(bwbis)
cover = sum(sum(bwbis))/(nx*ny)

csvwrite(strcat("/tmp/ramdisk/", imname, ".csv"), bwbis)


% Set image sizes 

p = 0.223;               % cell size (m)
slope = 0.6;            % slope angle (deg)
p_slope = p/cos(slope*pi/180); % cell size along the slope (m)

n=nx

FLmax = (n*(n+1)/2)*p_slope/n ;  % Maximum value of Flowlength



% Calculate flowlength
% simplified version for constant slope
% compact form, not optimized
%--------------------------------------------------------------------------

Flsum=0;
Flcol=zeros(1,ny);
for i=1:nx-1
    %bwstep=bw(i:nx,:);
    Flcol=Flcol+sum(cumprod(bw(i:nx,:)));
end
Flcol=Flcol+bw(nx,:);
Flsum=sum(Flcol/nx)/ny

FL=Flsum*p_slope


% Expectation from null model (Paco)
% according to Paco's notations
% p = cover
% L = n % nb of pixel along the y-axis
% ps = p % pixel size
d = p_slope ;

E_FL_r = (1-cover)*(cover*n-(1-cover)*(1-(1-cover)^n))*d / (cover*cover*n) %% Equation (1) in Rodriguez et al
E_FL_r2 = (1-cover)*(cover*cover*(1-cover)*(n+1)*(n+1)+cover*cover*n-6*(1-cover)+(1-cover)^(n+1)*(cover*cover*(2*n*n-1)+6*cover*n+6))*p_slope*p_slope/(cover^4*n*n) ; %% Equation (1) in Rodriguez et al
V_FL = E_FL_r2 - E_FL_r*E_FL_r ; %% variance of the Flowlength index
SE_E_FL_r=sqrt(V_FL/ny); %% standard error of the Flowlength index

toc

% Plot all the images
%--------------------------------------------------------------------------
figure

subplot(1,2,1)
image(Image)
axis off
axis square
title('Original','FontSize',12,'FontWeight','b','Fontname','Times')

subplot(1,2,2)
black = [0 0 0] ;
white = [1 1 1] ;
Satecolormap = [black; white] ; % 0=noir 1=white
colormap(Satecolormap) ;
image(bw)
axis square
title('Optimal graythresh','FontSize',12,'FontWeight','b','Fontname','Times')
xlabel({['Cover=' num2str(cover)], ['Flowlength=' num2str(FL)], ['Expected random=' num2str(E_FL_r)],['Stdm=' num2str(SE_E_FL_r)]});
set(gca,'xtick',[],'ytick',[])

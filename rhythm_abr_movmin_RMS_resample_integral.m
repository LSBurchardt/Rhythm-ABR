function rhythm_abr             %main function


%% 01: GLOBALS
% definde global variables

 global listOfFileNames

%% 02: DATA MANAGMENT
% load data
% Initialization steps:

workspace;  % Make sure the workspace panel is showing.
format long g;
format compact;

% Define a starting folder.
start_path = fullfile(matlabroot, '\toolbox');
if ~exist(start_path, 'dir')
	start_path = matlabroot;
end
% Ask user to confirm the folder, or change it.
uiwait(msgbox('Pick a starting folder on the next window that will come up.'));
topLevelFolder = uigetdir(start_path);
if topLevelFolder == 0
	return;
end
fprintf('The top level folder is "%s".\n', topLevelFolder);

% Specify the file pattern.
% Get ALL files using the pattern *.*
% Note the special file pattern.  It has /**/ in it if you want to get files in subfolders of the top level folder.
% filePattern = sprintf('%s/**/*.m; %s/**/*.xml', topLevelFolder, topLevelFolder);
%filePattern = sprintf('%s/**/*.xlsx', topLevelFolder); %for .xlsx data
%filePattern = sprintf('%s/**/*.xls', topLevelFolder); %for .xls data
filePattern = sprintf('%s/**/*.mat', topLevelFolder); %for .mat data
allFileInfo = dir(filePattern);

% Throw out any folders.  We want files only, not folders.
isFolder = [allFileInfo.isdir]; % Logical list of what item is a folder or not.
% Now set those folder entries to null, essentially deleting/removing them from the list.
allFileInfo(isFolder) = [];
% Get a cell array of strings.  
listOfFolderNames = unique({allFileInfo.folder});
numberOfFolders = length(listOfFolderNames);
fprintf('The total number of folders to look in is %d.\n', numberOfFolders);

% Step two: Get a cell array of base filename strings. 
listOfFileNames = {};
listOfFileNames = {allFileInfo.name};
totalNumberOfFiles = length(listOfFileNames);
fprintf('The total number of files in those %d folders is %d.\n', numberOfFolders, totalNumberOfFiles);

prompt = 'Assign a name for saving. Example: C_persp_IC for Isolation Calls of Carollia perspicillata. The name needs to be put in aposthrophes. ';
savename = input(prompt);
fprintf(['The name you chose for saving is ' savename]);

%% 03: ANALYSIS: Scaling, MMS, Integral, Resampling
[m,~] = size(listOfFileNames);
results = cell(m, 8); %empty cell array with rows for every file and 5 columns for variables we want to save
 
fs = 19200;               %sampling rate after 20-fold resampling
bma200_input_gain = 10000;  
abr_scale_factor = 1/(7e-5*bma200_input_gain);    %in mV, adopted from Lattenkamp et al, 2020 

% number of bootstraps for statistics
nboot = 500;                %number of resample sequences
abr_display_ylim = 0.01;      %in mV
abr_display_xlim = [0 20];    %in milliseconds
ana_wind_start = 0.0;         %offset of rms calculation window in sec (0 ms)
ana_wind_dur = 0.01;          %duration of rms calculation window in sec (10 ms)
%ana_wind_starts=round(ana_wind_start*fs);
ana_wind_starts = 1;          % start sample of analysis window (only if start = 0.0, otherwise line above)
ana_wind_stops=round((ana_wind_start+ana_wind_dur)*fs);
current_level = 90;           % Sound Pressure in dB of Stimulus Presentation


 for k= 1: length(listOfFileNames)
     
      matfilename = listOfFileNames{:,k};
      load(matfilename, 'rxin', 'modrate', 'batid', 'batsex');
      clear confidence confidencestd
      stimulus = matfilename(57:(end-4));

        data_scaled = rxin .* abr_scale_factor *1000; %scale factor incorporated, converted to mikroVolt
        mean_data = mean(data_scaled,2);
                      
        % data basis for resampling scaled, not averaged, all data
        data_re = data_scaled;
                
        %originalsignal: scaled, averaged signal, only analysis window
        originalsignal= mean_data(ana_wind_starts:ana_wind_stops,:);
        
        %%moving-minimum substraction method (Källstrand et al 2014)
        
        originalsignal(:,2)=movmin(originalsignal(:,1),12);                 %minimum in next 12 samples is found
        originalsignal(:,3)=originalsignal(:,1)-originalsignal(:,2);        %original data minus this minimum
        originalsignal(:,4)=originalsignal(:,3)-min(originalsignal(:,3));   % minima are alined to 0 by caluclating all minima minus the overall minimum
                                                                            % therefore baseline at 0 --> same procedure as in Källstrand et all 2014, see figure 1
    
        y= originalsignal(:,4);
        x= 1:length(y);
        
        %calcualte parameters for original data (scaled, averaged, only
        %analysis window)
        originalstd = std(y);
        originalrms = rms(y);
       
        %numeric integration of scaled, averaged data only in analysis window
        z = trapz(x(ana_wind_starts:ana_wind_stops),y(ana_wind_starts:ana_wind_stops)); 
      
%       % do a resampling analysis, Variante München
        for bn=1:nboot
            clear redata redata_mean y_re
            
            for rep=1:size(data_re,2)
                redata(:,rep)=circshift(data_re(:,rep),ceil(rand*size(data_re,1))); %resampling approach adopted from Lutz Wiegrebe, Lattenkamp et al 2020
            end
            
            %same analysis for every resampling: averaging over 256
            %repetitions, than moving minimum substraction, rms and std
            %over transformed signal
            
            redata_mean = mean(redata,2);
            redata_mean(:,2) = movmin(redata_mean(:,1),12);%zahl nach dem komma vorher 4
            redata_mean(:,3) = redata_mean(:,1)-redata_mean(:,2);
            redata_mean(:,4) = redata_mean(:,3)-min(redata_mean(:,3));
            
            y_re = redata_mean(:,4);
            
            resamplestd(bn)= std(y_re(1:192)); %resample parameter std
            resamplerms(bn) = rms(y_re(1:192)); %resample parameter rms
            
        end
            
        confidence = numel(find(originalrms > resamplerms))/nboot;
        confidencestd = numel(find(originalstd > resamplestd))/nboot;
                
       %%% Save results
       results(k,1) = cellstr(batid);
       results(k,2) = cellstr(batsex);
       results(k,3) = num2cell(modrate);
       results(k,4) = cellstr('BadSegeberg'); %change dependent on stimulus typ to analyse: IC short; IC long; tone pip
       results(k,5) = num2cell(z);
       results(k,6) = num2cell(originalrms);
       results(k,7) = num2cell(confidence);
       results(k,8) = num2cell(originalstd);
       results(k,9) = num2cell(confidencestd);
       results(k,10) = cellstr(stimulus);
       results(k,11) = cellstr(matfilename);
        
       %save the raw signal of moving minimum substraction for control and
       %plotting purposes
       movmin_results(1:length(y),k) = y;
       
end 
%save the results and the results of the moving minimum substractiom, saved
%in "mean_data"
save(['Rhythm_ABR_' savename '.mat'], 'results','movmin_results');
xlswrite(['Rhythm_ABR_' savename '.xlsx'], results);

end
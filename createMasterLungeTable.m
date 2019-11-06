cf = pwd; try cd([vol ':/' folder]); catch; end
[filenames,fileloc]=uigetfile('*LungeTable.mat', 'filter time files to analyze','multiselect', 'on'); %look below to save time to make truncated files okay
if ischar(filenames); filenames = {filenames}; end
cd(fileloc);
LungeTableAll = table();

for i = 1:length(filenames) %finding LungeTable files of the selected whales
    clearvars -except filenames fileloc i LungeTableAll
    filename = filenames{i};
    load([fileloc filename]);
    
    k = strfind(filename,'LungeTable.mat');
    whaleID=filename(1:k-1); %making it to that the characters before LungeTable are read, so that we can connect that to a prh file
    
    n = size(LungeTable, 1);
     
    LungeTable.whaleID(1:n,1)=repmat({whaleID},n,1);
    if i == 1;
        LungeTableAll = LungeTable;
    else
        LungeTableAll = [LungeTableAll;LungeTable];
    end
end
 
writetable(LungeTableAll,[fileloc 'masterLungeTable.csv']);
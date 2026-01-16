clc
clear
%循环读取tif文件
baseFileName = 'E:/UTCI/Result2/MIntensity_mean_clip/HS_mean';
version = '.tif';
numFiles = 10; 

dataCell = cell(1, numFiles);

% 循环读取每个文件
for i = 1:numFiles
    % 构建当前文件的完整路径
    filePath = fullfile([baseFileName, num2str(1980 + i), version]);

    % 读取栅格数据
    data = imread(filePath);

    % 存储到 cell 数组中
    dataCell{i} = data;
end

% 获取栅格的大小
[row, col] = size(dataCell{1});

% 初始化结果存储数组
result_mk = zeros(row, col);
result_sen_slope = zeros(row, col);


% 遍历每个像元进行MK检验和Sen's Slope方法的计算
for i = 1:row
    for j = 1:col
        % 获取当前像元在所有年份的数据
        pixelData = cellfun(@(x) x(i, j), dataCell);
        pixelData = double(pixelData);

        % 进行MK检验
        H_mk = mannkendall_test(pixelData);

        % 进行Sen's Slope方法计算
        sen_slope = senslope(1:numFiles, pixelData);

        % 将结果存储到对应位置
        result_mk(i, j) = H_mk;
        result_sen_slope(i, j) = sen_slope;
    end
end

%% 
% 获取参考信息（假设所有栅格文件的参考信息相同）
info = geotiffinfo(fullfile([baseFileName, num2str(1981), version]));

outputFile1 = 'E:/UTCI2_figure/Chapter2.2/slope_y10/mk_extreme1.tif';
% 写入结果到GeoTIFF文件
geotiffwrite(outputFile1, result_mk, info.RefMatrix, 'GeoKeyDirectoryTag', info.GeoTIFFTags.GeoKeyDirectoryTag);

outputFile2 = 'E:/UTCI2_figure/Chapter2.2/slope_y10/sen_extreme1.tif';
geotiffwrite(outputFile2, result_sen_slope, info.RefMatrix, 'GeoKeyDirectoryTag', info.GeoTIFFTags.GeoKeyDirectoryTag);
% 显示完成信息
disp('Results written to GeoTIFF file.');
%% 
function H_mk = mannkendall_test(data)
    n = length(data);
    s = 0;

    for i = 1:n-1
        for j = i+1:n
            s = s + sign(data(j) - data(i));
        end
    end

    var_s = (n*(n-1)*(2*n+5))/18;

    if s > 0
        Z_mk = (s - 1) / sqrt(var_s);
    elseif s < 0
        Z_mk = (s + 1) / sqrt(var_s);
    else
        Z_mk = 0;
    end

    H_mk = abs(Z_mk) > norminv(1 - 0.05/2);
end
function sen_slope = senslope(x, y)
    % 初始化斜率数组
    n = length(x);
    slopes = zeros(1, n*(n-1)/2);

    % 遍历所有可能的斜率组合
    idx = 1;
    for j = 1:n-1
        for k = j+1:n             
            slopes(idx) = (y(k) - y(j)) / (x(k) - x(j));
            idx = idx + 1;
        end
    end
    sen_slope = median(slopes);
end


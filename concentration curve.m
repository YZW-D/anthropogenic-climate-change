clc
clear
%% 
% filename = 'E:/UTCI/Result3/cluster/mean_all.xlsx'; 
filename = 'C:/Users/77236/Desktop/test.xlsx'; 
[num, text, raw] = xlsread(filename);
%% 
% 获取GDP和GreenArea列的数据
all2000 = num(:,[1,7]);
% all2000(:,4) = all2000(:,1)./all2000(:,3);
sorted_data = sortrows(all2000, 1);
n = size(all2000,1);
% 计算累积百分比
gdp = sorted_data(:,1);
all = sorted_data(:,2);
all_normalized = all;
all_normalized = normalize(all,'range');
k = 0:1:n;
k2 = k'/n;
cumulative_gdp = cumsum(gdp) / sum(gdp);
cumulative_all = cumsum(all_normalized) / sum(all_normalized);
cumulative_gdp = [0;cumulative_gdp];
cumulative_all = [0;cumulative_all];
% 计算基尼系数
area_under_lorenz = trapz(k2, cumulative_all);
area_perfect_equality = trapz(k2,k2);
gini_coefficient = 2*(area_under_lorenz-area_perfect_equality);

% 绘制洛伦兹曲线
figure;
% plot(k2, cumulative_all, '-', 'LineWidth', 2);
plot(k2, k2, 'Color', [0 0 0 0.65], 'LineWidth', 1.5); % 用红色虚线表示
hold on;
smoothed_curve = smooth(k2, cumulative_all, 0.1, 'rloess'); % 0.1 是平滑度，可以根据需要调整
plot(k2, smoothed_curve, '-','Color', [251, 131, 21]/255,'LineWidth', 1.5);
% plot(k2, smoothed_curve, '-','Color', [72, 179, 172]/255,'LineWidth', 1.5);
% plot(k2, smoothed_curve, '-','Color', [129, 95, 169]/255,'LineWidth', 1.5);
% 两条曲线之间填充斜线
x_fill = [k2; flipud(k2)];
y_fill = [smoothed_curve; flipud(k2)];
fill(x_fill, y_fill, [251, 131, 21]/255, 'FaceAlpha', 0.06, 'EdgeColor', 'none');
% fill(x_fill, y_fill, [72, 179, 172]/255, 'FaceAlpha', 0.06, 'EdgeColor', 'none');
% fill(x_fill, y_fill, [129, 95, 169]/255, 'FaceAlpha', 0.06, 'EdgeColor', 'none');
hold off;
% 设置图的长宽比例为1:1
pbaspect([1 1 1]);
% 设置背景为浅灰色
% set(gca, 'Color', [0.9 0.9 0.9]);
xlabel(['集中度指数：', num2str(gini_coefficient)]);
set(findall(gcf, '-property', 'FontName'), 'FontName', 'Times New Roman');
% 取消上轴和右轴的刻度
ylim([0,1]);
set(gca, 'XTick', [0 0.2 0.4 0.6 0.8 1], 'XTickLabel', {'0', '0.2', '0.4', '0.6', '0.8', '1'});
set(gca, 'YTick', [0 0.2 0.4 0.6 0.8 1], 'YTickLabel', {'0', '0.2', '0.4', '0.6', '0.8', '1'});
% 去掉网格线
grid on;
%% 
print('E:/UTCI2_figure/Chapter4/Chapter4.2.3/UM/mean_all2020_guiyi.png', '-dpng', '-r600');
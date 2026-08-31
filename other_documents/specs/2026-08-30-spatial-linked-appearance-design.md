# 数据集级细胞点外观设计

## 目标

每个数据集只配置一个 `point_size` 和一个 `point_opacity`。Projection、Linked views、Spatial、Gene expression、Trajectory、Trekker 和 Immune repertoire 的细胞散点都从同一配置初始化。

## 根因

- legacy `histology_image` 在 Spatial 页面被命名为 `Tissue background`，在 Linked Views 被命名为 `Embedded histology`，导致同一个 `spatial_image_settings` 叶子只在 Spatial 页面命中。
- Overview、Gene expression、Spatial、Trajectory 和 Trekker 各自读取或硬编码不同默认值。
- Linked views 还允许空间背景图的 alignment 覆盖细胞点外观，使同一数据集在不同页面显示不同。

## 设计

1. legacy embedded image 的公共名称统一为 `Tissue background`；canonical `histology_images` 的显式名称保持不变。
2. `createShinyApp()` 直接接收 `point_size` 和 `point_opacity`。每项可为一个数字，或名称与 `cerebro_data` 完全一致的数值向量/列表；不兼容旧页面专属键。
3. 生成配置以数据集名保存这两个值，运行时通过一个公共解析器获取当前数据集外观。
4. 背景图预设只负责图像几何和图像透明度，不再覆盖细胞点外观。
5. `point_size` 的公共单位是 CSS 像素直径；Canvas 仅在最终画圆时换算成半径。页面内滑块仍可临时调整当前视图。

## 验证

- 回归测试证明 legacy Xenium 图像能够命中 `Tissue background` 的 `flip_y` 和 `offset_y`。
- 回归测试证明数字会扩展到全部数据集，命名列表会按数据集解析，无效名称和范围会被拒绝。
- 回归测试证明 Linked views 不再从背景图读取点大小或透明度。
- 运行 createShinyApp、coordinated views、各 Viewer 页面和静态检查。

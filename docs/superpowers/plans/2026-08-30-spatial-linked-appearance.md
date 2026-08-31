# 数据集级细胞点外观实现计划

> **面向 AI 代理的工作者：** 必需子技能：使用 superpowers:subagent-driven-development（推荐）或 superpowers:executing-plans 逐任务实现此计划。步骤使用复选框（`- [ ]`）语法来跟踪进度。

**目标：** 保留 Xenium 图像对齐修复，并让一个数据集的所有细胞散点使用同一初始大小和透明度。

**架构：** 删除页面专属点外观键。`createShinyApp()` 校验并保存数据集级 `point_size` / `point_opacity`，公共运行时解析器供所有页面和 Linked bundle 使用。

**技术栈：** R、Shiny、testthat、现有 Canvas bundle。

---

## 任务

- [x] 为标量、命名列表、名称不匹配和越界值编写回归测试。
- [x] 修改 `createShinyApp()` 和 `launchCerebro()` 公共参数。
- [x] 增加当前数据集外观解析器，并接入各散点页面。
- [x] 删除 Spatial/Trekker 背景图对细胞点外观的覆盖。
- [x] 更新 PR2 demo 的每数据集配置。
- [x] 运行聚焦测试、完整测试和静态检查。
- [x] 确认未恢复 `trekker.js`、未 push。

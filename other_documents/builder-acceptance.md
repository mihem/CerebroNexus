# Builder 迁移与验收记录（2026-10-13）

范围：`codex/builder-on-master`，基线 HEAD `e2e94ef6`，master
`9b6ecc6f84f4223905b3cb2fd15ec7728571fd6f`。本次修复在工作区，未提交或推送。
这是一份带验证边界的记录，不是“所有参数完全匹配”或完整 CI 通过声明。

**后续修复：** [追加修复与复测](builder-followup.md) 记录后台事务、Windows/WSL
原生选择器、文案、安装布局和 Nix 的新结果。下文性能及“尚未验证”描述属于首轮记录，
不应忽略追加记录继续当作当前结论。

## 适用范围与优先级

采用 design-taste-frontend 的审计、信息层级和交互约束，以及
[Web Interface Guidelines](https://raw.githubusercontent.com/vercel-labs/web-interface-guidelines/main/command.md)
的表单、焦点、状态、响应式和减少动态效果检查。保留 Shiny、现有 JavaScript/CSS、
科研工作流和中性/橙色视觉；营销页面布局和更换框架不适用。

优先处理的问题：

1. **P1 正确性**：初始页面路由错配；运行时代码身份的路径别名误判；
   CRB 回调中 `identity` 被字符串遮蔽、连续队列缺少 reactive isolate；Spatial 换数据时旧颜色字段引发异常。
   全量测试还暴露了启动导航竞态：延迟到达的初始页面命令会覆盖读者刚点击的 Gene 页面。
   初始导航改为浏览器记录主动导航意图后不再执行，并覆盖连接前点击和条件页面延迟出现的情况。
2. **P1 持久化**：缺少作者显示默认值；批量恢复抢走已保存的数据集焦点；
   空间附件保存仍有主进程读取/复制；保存与刚刚提交的编辑竞争。
3. **P2 交互**：点击无即时反馈；保存成功还需关闭弹窗；构建前错误很快消失；
   Stop 按钮随进度反复替换；恢复缺少可用的取消入口。
   最终还统一了浏览器整数/浮点数的类型，去掉无变化的外观快照重复提交，避免 Save 误触发重新检查。
4. **P2 验收**：点大小下限与 Viewer 不同；标签边缘裁切；克隆画布共享错误的
   无障碍描述；旧测试仍假设 master 已经替换的控件或字段含义。

## 分支关系与迁移清单

比较对象：PR7 `cb770fe9`、lifecycle `d09a8733`、build `ab2f7d40`、
open-subtract `1ea60a95`。master 是当前分支祖先；PR7 不是当前分支祖先。
lifecycle 是 open-subtract 的祖先，不能把两者工作量相加。当前分支是在 master 上
重组导入的 8 个提交，不是把旧分支整树覆盖回来。
收尾时重新执行 `git fetch mihem master`，远端仍为上述 `9b6ecc6f`。
六个分支两两检查，只有 `master → 当前分支`、`lifecycle → open-subtract` 两条祖先关系；
build-streamline、PR7 与当前分支之间不存在直接包含关系。
当前分支与 PR7 的共同祖先为 `cbd65fe0`；三个 streamline/open 分支与当前分支、
PR7 的共同祖先均为 `33145024`。因此以下清单以行为去重，而非将分支提交数相加。

对 12 个关键运行时文件的顶层函数做了 AST 对照：PR7 的 590 个函数中，最初审计时
552 个相同、38 个变化、0 个缺失。这仅定位迁移范围，下面的行为证据才是保留判据。
CSV 原始证据为验收目录内 `migration-functions.csv`，不代表整个代码库函数总数。

| 工作/行为 | 判定 | 当前实现和证据 |
|---|---|---|
| 隔离导入、按序任务、后台 Seurat 对象 | 已保留 | `inst/builder/worker.R`、`session.R`、`server/imports.R`；import-queue、worker-app、adapters 测试；三个示例实际导入 |
| 预热空 worker、按需能力初始化 | 已保留 | `server/foundation.R` 的 prewarm、`worker.R` 的 capability registry；异步启动测试 |
| 独立异步任务调度、轮询、断开清理 | 已保留 | `async.R`、`server/foundation.R`；async、worker lifecycle 测试 |
| 源文件指纹、只在变化时读/哈希 | 已保留 | `adapters.R` snapshot cache、`project.R` source context/identity cache；adapters/project 测试。不同平台文件系统元数据开销仍需测量 |
| Save 保留源数据，Build 发布产物 | 已保留 | `project.R` 的 source jobs、checkpoint、manifest；project-hydration、project-source-sync 测试；实际 Save/Open |
| 空间图片附件的验证、复制 | 遗漏已补齐 | `builder_project_attachment_jobs/stage_attachment_jobs/apply_attachment_results`；把任务发给已有附件 worker，保留前后指纹、解码和目标验证；真实 callr 附件测试 |
| Open 轻摘要、CRB 直用、Source 按需恢复 | 已保留并修复 | `server/project.R` 的异步扫描、恢复选择、批量恢复；实际 3 CRB 快速恢复、25 万细胞后台恢复、取消和重试 |
| 保存的当前数据集/页面 | 遗漏已补齐 | 批量恢复不再把最后完成的数据集设为当前项；显式 Load source to edit 仍切换到所选对象；浏览器恢复后仍选中 ds2 |
| CRB/checkpoint 复用、仅重建必要部分 | 已保留 | `builder_project_artifact_matches_entry/entries_for_build`、`plan/freeze.R`；project/build/end-to-end 测试；三次 UI 构建的三个 CRB SHA-256 相同 |
| 作者显示改动与数据构建身份分离 | 遗漏已补齐 | `builder_project_artifact_configuration_digest` 排除 `viewer_display`；Build 配置仍冻结该字段，不复用过期显示配置 |
| 更少序列化、复制、重复校验 | 已保留 | worker 保存对象、轻量快照、缓存摘要、复用 CRB；保留目的文件校验、路径边界、runtime identity，不通过吞错换速度 |
| 原有输出冲突弹窗/二次确认 | 被更好的实现替代 | 每次 Build 生成唯一 release 目录；仍有 preflight、stage、锁、父进程校验与原子发布；不恢复旧的重复确认函数 |
| 旧 hydration/status helper、旧 file-cache helper | 被当前契约替代 | 当前 durable project、typed restore actions、source context、checkpoint identity；不能把其他分支中“同名函数缺失”直接判为功能遗漏 |
| 取消、失败保留旧产物、清理、重试 | 已保留并补齐 | 原 release settlement 取消/恢复继续保留；新增 worker 阶段协作取消标记及完成前检查；真实 Stop→未发布→Retry Build 成功 |
| 主线程反馈、渐进列表、预览 | 已保留并补齐 | `www/builder.js` 状态反馈、稳定按钮节点、selectize 解锁；`preview.R`/spatial alignment；浏览器和 Node 行为测试 |
| master Viewer Pack、canonical indices、首帧/空间坐标优化 | 已保留 | 不回退 Viewer；projection-first-frame-performance、spatial、binary-panel-values、generated-App 测试；修复了旧颜色字段竞态 |
| 将 qs 作为 R 4.6/Nix 必装依赖 | 刻意排除 | 已实际复现 pin 中 qs 编译/加载不兼容；移至 Enhances。兼容环境仍可读 QS，并提示转 RDS/QS2；不使用 allowBroken |
| 已移除的 IR 字体/图例大小等空操作控件 | 刻意排除 | `immune_repertoire/param_spec.R` 注释明确 Canvas/Plotly 不消费；不恢复无效 UI |
| 全平台原生选择器、生产规模内存峰值、1M 真实数据 | 尚未验证 | 后续已修复并实际验证 Windows 文件框到 WSL 的路径返回，见追加报告；全平台/文件夹框尚未全面验证。25 万细胞及大文件压力测试为合成数据，不能外推真实百万细胞场景 |

## Builder → Viewer 设置覆盖

链路简称：**A** = `ui/core_stage/stage.R` 控件 → `server/datasets.R` 带数据集/epoch
校验的状态更新 → `project.R` typed JSON 保存/恢复 → `plan/freeze.R` →
`app_bundle/request.R`、`contract.R` 的 `viewer_content` → Viewer 消费。
`R/viewer_content_contract.R` 与 `inst/viewer/core/viewer_content_contract.R` 必须逐字一致。

类别：**1** 链路已实现；**2** 控件存在但链路断开；**3** Viewer 可用而 Builder 缺入口；
**4** 浏览时临时交互，不应全部塞入项目。代码/契约测试与浏览器视觉验证分开记录。

| 页面/参数 | 类别与链路 | 语义、默认值、验证边界 |
|---|---|---|
| 数据集显示名、物种、分组、默认分组、分类颜色 | 1，既有 Core/settings→CRB/config→Viewer | 实际修改名称及分组色；颜色是类别映射，不是连续色阶 |
| 初始数据集、初始页面、欢迎文字 | 1，Build options→manifest→app plan→config→初始导航 | 修正 Projection=`overview`、Linked views=`coordinated_views`、Trekker=`trekker`；旧 checkpoint 页面目录升级；独立 Viewer 初始 Linked 已验证 |
| 投影包含集合、首屏集合、默认投影 | 1，Projection gallery→settings→plan→CRB/config | 主投影与首屏多个投影不同；预览与导出坐标测试、实际 UMAP |
| 点大小、透明度、显示细胞比例 | 1，Projection gallery→overview_*→app config→`viewerScatterDefaults` | 大小 1–20，透明度 0.1–1，比例 10–100%；Builder 原下限 0 已修为 1；实际 2/.3/100 恢复并在 Viewer Settings 验证 |
| 点形状 | 无通用作者控件 | 当前共享 cell Canvas 绘制点，不提供通用可配置 symbol；不伪造一个 Viewer 不消费的入口 |
| Linked 分组标签、点边框、正方形 | 3→1，A→coordinated bundle→`cell_views.js` | 默认 true/false/false；仅数据身份改变时播种，不因重新收到 bundle 清掉读者交互 |
| Linked 标签字号、字体 | 3→1，A→display_defaults→Canvas 标签绘制 | 11 CSS px，8–24；system-ui/Arial/Georgia。18 px Georgia 实际显示；边缘标签限位。不是 Plotly pt，也不是全站字体 |
| Linked 连续色域 | 3→1，A→color_clip→Canvas | full、1–99%、2–98%、5–95%；数值保存 0/.01/.02/.05；不是固定表达量上下限 |
| Gene 与 Spatial feature 色阶 | 3→1，A→gene_expression→Viewer 配色解析 | Cerebro orange/blues/greens/viridis；Viridis 实际显示；与分类颜色分别持久化 |
| Gene 多面板配色模式、面板 palette | 3→1，A→gene_expression | shared/distinct，default/soft；typed config 测试，未逐个模式截图 |
| Projection 标签、边框、正方形 | 3→1，A→`overview/*` 和共用 border UI | 默认 true/false/false；false/false/true 在生成 Viewer 实际核对 |
| Gene 边框、正方形、绘制顺序 | 3→1，A→`gene_expression/*` | false/false/Highest expression on top；后者也可 Random；Gene 无分类分组标签开关 |
| Spatial 标签、边框、正方形、分组区域轮廓 | 3→1，A→`spatial/obj_projection_parameters_plot.R` | 均 false；外观 patch 不重建点数据，区域轮廓因需要几何另行 reactive；读者显式偏好保留 |
| Trajectory 标签、边框、正方形 | 3→1，A→`trajectory/projection*.R` | true/true/false，保留该页边框的不同默认值 |
| Gene 伪时间趋势线显示、线宽 | 3→1，A→`UI_expression_by_pseudotime.R` | true、2，线宽 1–10 为 Plotly 绘图单位；实际生成配置 4，真实 renderUI 回归验证。平滑带宽仍属分析交互（4） |
| Trajectory 密度透明度、趋势线显示/线宽 | 3→1，A→`distribution_along_pseudotime.R` | .6、true、2；透明度 0–1，线宽 1–10。浏览器 Save 后生成 Viewer 实际为 .3、false、5；修复原 `req(FALSE)` 阻断整张图，Plotly 回归验证关闭/开启均正常 |
| IR Clonal UMAP 标签、边框、正方形 | 3→1，A→IR settings/visualizations | true/false/false；点大小/透明度/比例使用公共 cell-scatter 默认值 |
| IR Scatter/Paired Scatter 大小、透明度 | 3→1，A→IR display spec | 大小 .1–6、默认 1；透明度 .1–1、默认 .8；绘图库单位，不宣称与 CSS 像素相同 |
| HLA 图例模式、节点倍率 | 3→1，A→`hla_param`/settings/visualizations | auto/always/never，倍率 .3–2.5；Hide 与 Auto 分别实测。数值输入后立即 Save，磁盘配置和独立 Viewer 都为 1.7；不能仅以控件显示值为证 |
| Spatial 图片、标签、多图、图像原始 bounds | 1，spatial gallery→images→Save 附件 worker→app spatial_images | 附件内容/路径闭包验证；区分 embedded/外部图片；不能按文件同名认定相同 |
| 图片 X/Y 位移、X/Y 缩放、旋转、翻转、透明度 | 1，alignment draft→settings→spatial_image_settings→`spatialImagePreset` | 位移是空间坐标单位，旋转是度，缩放是倍率，透明度 0–1（Builder 输入 0–100）；geometryBaked 避免重复变换。实际修改图像 35°、55%、Flip X，保存并在独立 Viewer 选中 Arrow background 后核对图像；不是像素逐点相等断言 |
| Spatial 点大小、透明度、ROI 旋转与局部外观 | 1，alignment/ROI→spatial_*_settings→Viewer | ROI 作用域含 dataset/section/ROI，不能套用全局点大小；空间 contract 测试，部分浏览器画面核对 |
| 坐标轴、全站背景、面板标题样式 | 产品固定样式/数据派生 | 现有 Canvas 没有通用作者接口；面板标题由投影/空间名称决定。没有新增无效 CSS 文本输入 |
| 各页缩放、平移、框选、套索、Focus、当前 cohort | 4 | 浏览状态；应用 share/view snapshot 的职责与 Builder 作者配置不同 |
| Linked 当前空间选择、clonal layout、hover | 4（初始投影集合另为 1） | 读者探索；切数据集不能把旧结果写入新图 |
| Gene 当前基因/基因集、表达详情分组、过滤器 | 4 | 分析选择；作者负责可用数据/基因列表/默认分组，不冻结一次浏览结果 |
| Spatial 当前 sample/FOV/ROI、Feature/RGB、split-by | 4（ROI 几何/外观另为 1） | 读者导航；测试覆盖切换与复位。图片校准是作者配置，当前视口不是 |
| Trajectory 方法/名称的可用集合与默认项 | 1，trajectory gallery→CRB/config | 浏览中临时换轨迹/配色/selection 属于 4 |
| IR 链、克隆定义、图种、阈值、重采样、分组顺序 | 4 | 分析参数；与作者设置的 IR 源、保留内容、显示默认值不同 |
| HLA 链、网络scope、V分割、最小motif、孤立节点、关联筛选 | 4 | 分析参数；不通过展示设置改变统计解释或推断限制性 |
| Trekker 坐标朝向、QC筛选、证据面板、Moran基因选择 | 4；数据/嵌入证据保留为 1 | 当前 Builder 不新增每个探索步骤的“初始分析答案” |
| Groups、Marker、Most expressed、Pathways、Extra material 页的表格选择/过滤/排序/下载 | 4；可用内容/表格为 1 | 保留 master 对无单位 pct 的 Stored score 文案，不重新标成百分比 |
| WebGL、hover details、浏览器缩放、减少动态效果 | 4/用户偏好 | 不作为数据集科学配置持久化；遵循现有设置与系统偏好 |

未发现仍保留的已知“显示控件有 UI、但完全不消费”的作者控件；这不是对所有动态分析控件的无遗漏保证。
尤其不能把 34 个新增字段的 schema 往返测试说成 34 项视觉效果都已逐一验收。

## 操作与响应性

测试数据和产物均位于 `/home/wang/builder-acceptance-20261013`，独立端口会话；
Windows 证据目录为 `C:/Users/wangx/Projects/CerebroNexus-backups/2026-10-13-builder-acceptance`。

- 首次：Load example datasets → 配置三例 → Finish Checking → Save 选择项目目录。
  Save 保留工作及源文件；checked CRB 可继续后台准备，成功用非模态通知。
- 再次：点击 Save；不需要再次确认“保存成功”。失败保留上个完整项目并提供重试。
- Open：选 builder-project.json → 轻摘要 → 每项选择 CRB/Source/Skip → Open Project。
  CRB 不要求读入原对象；Source 后台恢复。可取消，已经完成的项保留，迟到结果按队列身份丢弃。
- Build：Review → Continue to Build → 保留上次 App 设置 → Build 选择父目录。
  创建唯一 release；再 Build 可复用 CRB；错误留在 Build 页，Retry Build 直接重试。
- Stop 为协作取消：当前不可中断步骤结束时停止，在发布前再次检查；不是立即强杀正在写文件的 worker。
  取消阶段不会发布；重试已实际成功。发布临界区仍使用已有安全终止/回滚协议。

probe.js 用捕获阶段 click、可见状态 DOM mutation 后 requestAnimationFrame 测首次反馈，
同时采集 Long Task、100 ms 浏览器定时器和 100 ms Shiny 心跳。
这是带性能探针的单次/少量 WSL 测试，存在并行测试负载，不是生产 p95 或受控基准。

| 操作 | 最新一次首次反馈 | 范围/限制 |
|---|---:|---|
| Open | 2.6 ms | 点击后的真实“Opening…”状态，不是项目全部就绪 |
| 确认 Source 恢复 | 3.9 ms | 修复前记录有 1.9–2.6 s；后端仍有长 flush |
| Save | 13 ms | 不是源文件复制结束时间 |
| Finish Checking | 7.3 ms | 后续校验仍在进行 |
| Continue to Review | 9.2 ms | 不代表全部审核模型计算在该时间完成 |
| Build | 7.5 ms | 打开目录选择过程的反馈 |
| 选择输出目录 | 82.3 ms | Preparing build 状态 |
| 25 万细胞源恢复 | 约 6.6–7.2 s 完成，重试热读约 1.1 s | 合成稀疏 250k×1000，约 50 MB RDS；不能当成真实百万细胞结果 |

上表来自 `metrics-fa6a5017691a3c0ec882618e7b2fd1ea.json`：5 个浏览器长任务，最大约 95 ms；Shiny 心跳最大间隔约 3.33 s。
最后会话 `metrics-192476bdc7b476c5bf3fb13a9e6ff6d1.json`（不启用 Rprof）的对应反馈为：
Open 16 ms、Source 确认 13.2 ms、Save 14.2–17 ms、Finish Checking 11.2–13.2 ms、
Review 10.2 ms、Build 5.1–18.1 ms、输出目录确认 86.8–119.9 ms。
它记录 4 个浏览器长任务、最长 88 ms；服务端心跳最大间隔 **4.84 s**，附近没有浏览器定时器长间隔。
`gap-report.py` 将最长间隔定位在 Build queued/Start worker 交界；Save 为约 2.6–2.7 s。
源码位于 Windows 挂载目录、服务在 WSL；存在并行测试负载，不能直接归因或外推生产性能。
长间隔集中在 Source 合并、Save 和构建准备。因此“首次反馈”目标在所测操作达成，
“服务端始终响应/全部操作流畅”未达成，仍需优化重模型合并与冻结期间的主进程工作。
原生 Windows/macOS 选择器未实测；WSL 的 server-path 后备入口已实测。

补充对照：`metrics-ext4-final.json` 来自源码位于 Linux 本地磁盘的新会话。
Open 11.6 ms、Source 确认 14 ms、Save 16 ms、Finish Checking 7.1 ms、
Review 12.6 ms、Continue to Build 164.8 ms、Build 17.8 ms、输出目录确认 114.9 ms。
浏览器记录 1 个 99 ms 长任务，Shiny 最大间隔仍有 3.15 s，Save 为 2.44 s。
`runtime-hash-timing.csv` 单独测量同一身份检查函数（252 个运行时文件）：
Windows 挂载目录三次为 2.471/2.177/2.167 s，Linux 本地目录为 .039/.013/.015 s。
这证明文件系统对同步身份检查有显著影响，也证明它不能解释全部卡顿。
没有缓存掉最终身份检查；将其后台化还需要保持父进程信任边界与取消语义，当前未完成。

独立 Save 采样（`save-main.Rprof`、`save-main-profile.csv`）进一步显示：
`complete_builder_project_save_request` 累计约 2.22 s，其中 `builder_project_write`
约 1.79 s；配置完整性核验涉及 JSON 重新规范化。另有 CRB 完成登记约 1.47 s。
这些是采样调用栈的包含时间，不能相加当作一次 Save 总耗时；采样本身和首次编译也有开销。
它把剩余问题定位到主进程事务核验/登记，而不是推断“只要 worker 异步就不会阻塞”。

## 界面证据与验证边界

证据目录保存：`viewer-linked-verified.jpg`（18px Georgia 与边缘限位）、
`viewer-gene-viridis.jpg`、`viewer-projection-defaults.jpg`（2/.3 与作者开关）、
`open-crb-summary.jpg`、`missing-source.jpg`、`large-retry-success.jpg`、
`build-cancelled.jpg`、`build-retry-success.jpg`、`spatial-switch-fixed.jpg`。
浏览器看到缺失源只能 Skip，没有误标 Build-ready；250k 早期取消后旧结果没有再出现。

新增证据：`checkpoint-queue-ready.jpg`（连续后台 CRB）、`spatial-alignment-edited.jpg`、
`viewer-spatial-image.jpg`、`viewer-hla-1_7.jpg`、`viewer-linked-narrow.jpg`、
`viewer-settings-narrow.jpg`。最终 Canvas 的五个 aria-describedby 各指向本面板，
没有残留的 waiting 文本或其他面板描述。CSS 抽屉使用不透明背景，避免底层文字透出。
`viewer-hla-first-open-fixed.jpg` 记录首次打开抽屉在分析初始化后仍保持打开；
`builder-pseudotime-defaults.jpg`、`viewer-pseudotime-defaults.jpg` 对照最终伪时间设置。

| UI 项目 | 实际检查与边界 |
|---|---|
| 空/加载/成功/错误/禁用 | 空 Builder、后台恢复、缺失源、构建停止/重试、完成状态均实际查看 |
| 层级与动作 | 高级作者参数折叠；Save 成功不阻断；错误与 Retry 保留在页面 |
| 窄窗口 | 760×850 的 Builder 路径弹窗、Viewer 导航、Linked 与 HLA 设置已查看；1280×720 检查正常工作区 |
| 焦点与键盘 | Viewer Settings 的 Escape 关闭并回到 hla_more_button；路径弹窗改为允许 Escape，独立组件浏览器验证关闭 |
| 首次打开 HLA 设置 | 沿用 IR 的 sticky-in-page，防止内部分析 tab 初始化把刚打开的抽屉关闭 |
| 可读性 | 修复设置背景透字；标签边缘限位；没有宣称全站 WCAG 对比度与读屏器全通过 |
| 动效/缩放 | 保留既有 reduced-motion CSS；当前工具发送缩放快捷键后 width/DPR 未变化，倍率测试未完成；未模拟系统减少动态效果偏好 |

构建产物 `7adf6e`、`79f477`、`43968f` 对应首次、不变、仅改 App 配置的三次成功构建；
`artifact-reuse.csv` 提供三个 CRB 的 SHA-256，跨三次一致。
`ae1f85` 为新增逐页默认值后的产物；它与前面产物跨越代码/契约修复，不能要求字节相同。
`854ae7`、`2d96a4`、`f594b6` 为最后的设置/空间图像与 UI 修复产物；
`final-artifact-reuse.csv` 记录三次打包的 CRB 字节与 SHA-256，对应数据均复用。
`2ee265` 是补充伪时间作者设置后的独立 Viewer；该次恢复源数据后重新生成 ds1，
没有把它误计为“全部 CRB 未重建”。
`2cf388` 是包含最终导航修复的末次浏览器构建。独立启动后无需点击即进入作者指定的
Acceptance Complete / Linked views；Settings 实际为点大小 2、透明度 .3、100% 细胞，
18 CSS px Georgia 标签可见。证据为 `final-build-complete.jpg`、`final-viewer-settings.jpg`。

## 测试记录

一次完整、固定源码快照的测试结果为 **24418 passed、4 failed、3 error、2 warning、11 skipped**
（`frozen-source.log/results.rds`）。不能把此结果写为完整源码测试通过。
7 个失败/错误分别是：复制的源码缺少 Git 上下文、生成应用的首次导航竞态、
两个旧 worker 测试替身、恢复期间可浏览的旧断言、已删除的成功弹窗文案断言、
改成不透明抽屉后的旧 CSS 断言。前两项分别补 Git 上下文及修复导航行为；
其余更新测试契约，保存反馈增加实际 JavaScript 行为断言。
2 个 warning 为 Seurat 稀疏矩阵转换提示和复制源码中的 Git 调用；后者在带 Git 上下文复测。
11 个 skip 涉及不可加载的 qs、Windows/macOS 专属场景、未启用的压力测试、
缺少真实 Mouse 1M fixture，以及与本机已安装依赖互斥的缺依赖场景。
该全量快照之后的 HLA 抽屉、伪时间设置、数值规范化和导航修复使用下列定向回归；
它们不能合并冒充在最终源码上重跑的完整 CI。
最后一版显示/交互/页面定向测试为 **347 passed，0 failed/error/warning/skipped**，
包括真实 renderUI、Plotly 结果、typed JSON、附件后台任务和取消检查。
其后数字规范化/重复提交修复的受影响范围复测为 **170 passed，0 failed/error/warning/skipped**。
Project 与保存状态行为复测为 **878 passed，0 failed/error/warning，1 skipped**（本机没有 macOS 路径别名）。
旧 worker 测试替身修正及带 Git 上下文的 benchmark CLI 复测为 **131 passed，0 failed/error/warning/skipped**。
导航修复之后的 end-to-end、initial-navigation、review-feedback、package-check-contract
为 **775 passed，0 failed/error/warning/skipped**（`navigation-final.log/results.rds`）。
其中完成全部 15 种外部图片构建/迁移组合，以及 9 个独立生成 Viewer 的浏览器检查，
涵盖 embedded/HDF5/BPCells 与 plain/histology/Trekker。没有放宽原超时或省略失败断言。
静态收尾：66 个修改/新增 R 文件通过 Air，3 个修改的 JavaScript 文件通过 Node 语法检查，
Builder/Viewer 两份共享显示契约字节一致，`git diff --check` 通过。
74 个实现/测试/包文件与最终定向测试快照逐字节一致，记录在 `delivery-source-sha256.json`；
验收报告等文档另行维护，不计入该实现快照。
安装后的布局检查与 Linux launcher 的 **36 项断言通过**。
最后安装包的 `R CMD check --no-tests --no-manual --ignore-vignettes --no-build-vignettes`
为 **0 errors、0 warnings、1 NOTE**；NOTE 是原有 Imports 中 data.table、ggdendro、patchwork
未被使用。日志为 `delivery-display-install.log`；数字规范化之后另有 `canonical-install.log`，
最终导航修复后再次安装检查的 `navigation-install.log` 仍为 36 项断言通过、0 errors/0 warnings/1 NOTE。
不能将跳过测试的检查记作完整包检查通过。
开发过程中代码变化的运行不能标作最终完整 CI。
Nix evaluation/dry-run 已通过；完整 Nix derivation CI 尚未运行，本机需要大量本地构建，
且配置的二进制缓存未被信任。没有禁用安全校验、使用 allowBroken 或把 dry-run 当作 CI。
`R CMD check --no-tests --no-manual --ignore-vignettes --no-build-vignettes` 与完整包检查不同。

尚未覆盖：全平台原生选择器、真实 1M 数据、全部动态分析组合、所有倍率/缩放组合、
完整无障碍读屏器验收，以及修复后的生产负载性能分位数。不得据本记录宣称“全部参数可配置”。

## 交付入口与剩余验收门槛

- [迁移函数对照](C:/Users/wangx/Projects/CerebroNexus-backups/2026-10-13-builder-acceptance/migration-functions.csv)
- [构建复用 SHA-256](C:/Users/wangx/Projects/CerebroNexus-backups/2026-10-13-builder-acceptance/final-artifact-reuse.csv)
- [最终组合回归日志](C:/Users/wangx/Projects/CerebroNexus-backups/2026-10-13-builder-acceptance/navigation-final.log)
- [最终安装/包检查日志](C:/Users/wangx/Projects/CerebroNexus-backups/2026-10-13-builder-acceptance/navigation-install.log)
- [最终代码快照校验](C:/Users/wangx/Projects/CerebroNexus-backups/2026-10-13-builder-acceptance/delivery-source-sha256.json)
- [浏览器/服务端响应性原始记录](C:/Users/wangx/Projects/CerebroNexus-backups/2026-10-13-builder-acceptance/metrics-ext4-final.json)
- [窄窗口设置抽屉](C:/Users/wangx/Projects/CerebroNexus-backups/2026-10-13-builder-acceptance/viewer-settings-narrow.jpg)
- [独立 Viewer 的作者设置](C:/Users/wangx/Projects/CerebroNexus-backups/2026-10-13-builder-acceptance/viewer-pseudotime-defaults.jpg)
- [最终独立 Viewer 界面](C:/Users/wangx/Projects/CerebroNexus-backups/2026-10-13-builder-acceptance/final-viewer-settings.jpg)

本次功能修复有对应证据，但**不授予完整验收通过**。主要剩余门槛是主进程多秒停顿的事务级后台化、
最终源码完整 CI/Nix CI，以及上述平台与 UI 未测场景。将事务移到后台需要验证并发保存、关闭会话、
取消、重试和发布完整性，不能用复用未经验证的旧校验结果代替。

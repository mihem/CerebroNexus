# Builder 追加修复与复测（2026-10-13）

本文续接 [迁移和设置覆盖表](builder-acceptance.md)。保留原报告的分支、设置、
产物复用证据；旧报告中的性能、原生选择器和 Nix 限制应结合本文的新证据阅读。
修改仍在 `codex/builder-on-master` 工作区，未提交或推送。

最终可复核结果：Windows 文件框实际选中文件并返回 WSL；长提示已精简，完整原因可展开。
完整 Nix 源码测试 24,557 项通过、0 失败；完整包检查 0 ERROR、0 WARNING、3 NOTEs。
末尾小改动另经源码/安装后各 246 项定向验证。关闭 Rprof 的 250k 会话中点击反馈
约 5–172 ms，但冷恢复仍有约 1.84 s 的服务端心跳间隔，不能宣布全程无停顿。

## 已落实的修复

| 问题 | 修复与边界 |
|---|---|
| Save 的事务核验、JSON 规范化和序列化阻塞主进程 | `builder_project_write_process()` 在独立 R 进程执行完整事务。仍检查项目 revision、路径、源文件身份及最终文件；保留冲突、失败和回滚行为。 |
| CRB 登记在主进程重复读取和校验 | 后台核验、登记，再将已核验的 artifact closure cache 回传；缓存继续受身份/文件状态约束。 |
| Build 前准备阻塞 | 运行时代码身份、stage/preflight 准备放入异步进程；继续使用原有锁、完整性检查和发布协议。 |
| 导入回调带走整个实时 Builder 环境 | 回调在 worker 环境解析依赖，去掉 `srcref/srcfile`。实测四个回调分别约 3,530、1,249、2,381、170 字节；原先各约 3.5 MB。真实跨进程复制和登记测试通过。 |
| 首次使用集中触发 JIT | 会话开始前解析/编译 server 程序和实测热函数；不修改进程全局 JIT 设置。首次恢复仍有剩余停顿，见下表。 |
| 首次导入在主进程加载存储依赖 | worker 返回 BPCells/HDF5Array 能力标记，主进程不再为推荐后端加载这些包；Build 继续检查实际依赖。 |
| Windows/WSL 没有原生文件选择器 | WSL 调用 Windows PowerShell STA 文件框；返回路径经 `wslpath` 转换。手动输入 Windows 绝对路径也走转换及原有允许路径检查。 |
| 选择器等待状态无退路 | Open/Build 等待界面提供 Enter path 和 Cancel；只终止当前选择器，避免误杀其他服务。 |
| Retry Build 重新陷入选目录 | 失败/取消后复用已确认目标重试；实际 Stop → 未发布 → Retry Build → 成功。Build again 仍需选择父目录，不宣称重复 Build 已变成单击。 |
| 无变化的 Save 改写配置身份 | 不再给缺少 tables 的旧配置注入空列表；真实 Save 两次验证配置引用复用。 |
| 安装包中的身份验证函数找不到 | 在隔离运行时找不到函数时，从已安装命名空间定位同一验证器；没有绕开身份核验。 |
| 测试假设存在源码的 `R/`、`inst/` 和 DESCRIPTION | 测试资源优先源码、再定位安装目录；支持多个路径。独立安装布局复测，不用源码目录伪装安装测试。 |
| WSL 自动化测试可能弹出原生窗口 | 模拟目录选择器的测试局部清除 WSL 环境标记，确保使用测试命令；增加实际选择器规格断言，测试结束自动恢复环境。原生能力测试单独进行。 |
| Nix 中 BPCells 先加载后，HDF5Array 无法加载 | BPCells 改用 `pkgs.rPackages.Rhdf5lib.hdf5`，与 rhdf5 使用相同的 ROS3 HDF5 构建；同步修改 `create_env.R`。两个加载顺序均用真实 HDF5/BPCells 矩阵读写验证。 |

## 文案与状态

保留科学应用的现有风格与 Shiny 技术栈。按入口、导入、配置、恢复、保存、构建、
错误、Viewer 无结果提示和帮助文案检查；没有把详细科学说明一律截短。

| 场景 | 新行为/文案 |
|---|---|
| QS reader 不可用 | `QS unavailable. Use RDS or QS2.`；具体原因放入 Details。替代格式只列出当前可用 reader，QS2 也不可用时仅建议 RDS。Windows R 4.5 的 reader 可用，不能概括为所有平台不支持 QS。 |
| 手动项目路径 | `Open project` / `Project file (builder-project.json)`；用户主动选择 Enter path 时，不再误报原生选择器不可用。 |
| 保存 | `Saving project…` → `Saved`；修改后 `Unsaved changes`。去掉重复项目名；保存期间可以切换允许浏览的页面。 |
| 保存成功提示 | `Project saved.`；只有确实在准备 CRB 时追加数量和等待提示。没有缓存任务不能说成缓存已就绪。 |
| 构建失败/取消 | 短结论、可见重试入口；完整错误、阶段和路径留在 Details。 |
| 已有 CRB | `Saved CRB ready to build.`；编辑源数据的入口独立，文件路径折叠。 |
| Viewer 无结果 | 直接说明缺少什么或阈值未命中，并给出切换 method/grouping 的动作。 |
| 科学帮助 | 缩短冗余句式；保留数据单位、参考版本、预览抽样与最终 Viewer 的差异。 |
| Review 的预计体积/时间 | 删除没有可信依据的显示：原先用源文件大小估计已生成 CRB 的体积。保留实际构建进度与完整性检查。 |
| 检出的数据内容 | 不再把内部 `metadata_policy` 显示成一种科研内容；元数据配置入口保留。 |

另逐条复核了 96 条较长静态文案候选（`ui-long-text-review.csv`）。将选中细胞分布、
Spatial 分组筛选、Trajectory 投影和状态组成四段帮助缩到约 100–180 字符。
说明按实际绘图逻辑核对：轨迹状态使用独立选择的分组；百分比影响柱状图和表格，
Sankey 仍显示细胞数。保留有必要的科学单位、统计含义和限制。
该清单是源码审阅证据，不等于所有动态提示均已在浏览器触发。

键盘验收还发现 Bootstrap 的焦点规则覆盖了跳过导航链接的白色文字。
已在 `builder.editorial.css` 修正 hover/focus 颜色。实际浏览器读取的文字/背景
由 `#23527c/#171a18` 改为 `#ffffff/#171a18`，对比度由约 2.15:1 提高至 17.54:1；
Tab → Enter 可将焦点移入工作区（`keyboard-focus-fixed.png`）。

用户确认实际 Windows 文件框弹出，并选中临时 `picker-project/builder-project.json`。
R 返回 `status = "selected"` 和正确 `/mnt/c/...` 路径（`native-picker-result.txt`）。
这是用户协助完成的真实文件框测试；窗口自动化工具没有识别该原生窗口。
未据此宣称所有平台、文件夹选择框和全部 reader 组合均已验证。

## 浏览器复测与响应性

使用独立 Builder 7920、独立生成 Viewer 7921 和临时项目/输出目录。
复测三个示例项目的保存、三份 CRB 恢复、取消和重试构建；独立 Viewer 实际显示数据。
大数据是 250,000 cells × 1,000 genes 的合成稀疏对象，源文件约 47.9 MB。

反馈时间从点击到首个状态 DOM 更新后的动画帧；另用 100 ms 服务端心跳、浏览器定时器、
Long Task 和 Rprof 观察阻塞。心跳只记录 >200 ms 的间隔，不等于精准 R CPU 时间。
这是一台运行其他服务、部分测试并行的机器上的单次/少量运行，不是性能分位数。

| 场景 | 点击反馈 | 主进程/心跳结果 |
|---|---|---|
| 原报告基线 | 5–165 ms | 存在 3.15–4.84 s 停顿 |
| 后台 Save 后，三示例工作流 | 5–201 ms | 重复 Save 最大约 496 ms；首次 Save 加页面切换约 752 ms |
| 250k，回调传输修复后 | 19–147 ms | 保存窗口约 240 ms；首次恢复仍约 1.72 s |
| 250k，补充热函数编译后 | 17–149 ms；Save 20 ms，页面切换 81 ms | 保存窗口约 232 ms；冷恢复约 1.61 s。该次未记录浏览器 Long Task 或 >150 ms 定时器停顿 |
| 250k，后端能力检测移入 worker 后 | Open 13 ms，路径确认 155 ms，恢复 11 ms，Save 18 ms；Data 导航 257 ms | 冷恢复最大心跳间隔约 1.55 s；Save 加导航窗口约 468 ms。该次未记录浏览器 Long Task 或 >150 ms 定时器停顿；后台恢复耗时 9.8 s |
| 修复后的 Nix 环境，250k | Open 19 ms，路径确认 145 ms，恢复 20 ms，Save 14 ms，Data 导航 82 ms | 冷恢复最大心跳间隔约 1.35 s，保存窗口约 292 ms；恢复 8.8 s。未记录浏览器 Long Task 或 >150 ms 定时器停顿 |
| 最终补充：关闭 Rprof，250k | Open 5 ms，路径确认 172 ms，恢复 17 ms，Save 11 ms，保存期间 Data 导航 79 ms | 冷恢复最大心跳间隔约 1.84 s，Save 加导航窗口约 412 ms；恢复 12.4 s。未记录浏览器 Long Task 或 >150 ms 定时器停顿；当时仍有全量测试并行 |

因此后台事务和回调传输的阻塞已显著降低，但**不能称整个流程无停顿或全部达到 200 ms**。
剩余冷恢复涉及首次 observer 编译、轻量状态接收和配置界面生成。

其中一次导航仍超过目标，不能挑选较快的一次来声称达标。测量原文件依次为
`metrics-36a823064e9fe28f60efd0016cbe5558.json`、
`metrics-4a48376ca044e13da777dc4a55093f8f.json`、
`metrics-5e3c6c6e16c55927a5c66db526a8ee98.json`、
`metrics-7f5b66fb19641deb22c75fcffb4c615b.json`。
Nix 复测为 `metrics-8535eeaa7834c62601af085df84123df.json`。
关闭 Rprof 的独立会话为 `metrics-noprofile-a325e42d7b4bc4b03c36fe2b5472fe40.json`。
其冷恢复间隔更长，说明剩余停顿不能归因于 Rprof，也不能只报告 1.35 s 的较快结果。

### 已实际走通的操作

1. Open → Windows 文件框（用户协助）或 Enter path → 选择恢复的数据集。
   有效 CRB 可先恢复结构用于 Build；加载源数据后才能继续编辑。
2. 编辑后点击 Save；首次选择项目位置，此后直接保存。顶部显示 Saving → Saved，
   修改后显示 Unsaved changes。保存期间实际切换过 Data 页面。
3. Review → Continue to Build → Build → 选择输出父目录 → Build here。
   后台执行准备、构建、校验和发布。Stop 后没有发布；Retry Build 成功。
4. 独立启动生成目录内的 Viewer，实际检查 Projection 和窄窗口显示。
   首轮设置恢复和部分配置修改后的 Build 证据见迁移报告。

三次成功发布 `dfa159`、`794e7f`、`a0dcce` 中的三个 CRB 分别 SHA-256 完全相同
（`release-crb-reuse.json`）；这证明这些未修改数据产物得到复用，不代表任意项目都不重建。

关键截图位于同一证据目录：`large-final-saved.png`（250k、Saved 和简短 QS 提示），
`open-three-caches.png`（三份 CRB 恢复），`build-path-720.png`（窄窗口路径输入），
`build-complete-720.png`（构建成功），`missing-settings.png`（缺失配置），
`retry-build-success.png`（取消后重试），`viewer-720.png`（独立 Viewer）。

最终又实际检查了 480 × 800 窗口：长路径输入、三数据集恢复弹窗的内部滚动和固定底部按钮、
已恢复工作区、Dataset Manager 与 Review。键盘可提交项目路径，Escape 可关闭侧栏并将
焦点交回触发按钮。新增 `three-restored-480.png`；`three-final-saved.png` 和
`viewer-nix-projection.png` 分别记录最终保存状态及独立 Nix Viewer。
缩放快捷键在测试浏览器中未改变 viewport 或 devicePixelRatio，不能将这次尝试算作缩放验证。

## 测试证据

原始日志和截图位于
`C:/Users/wangx/Projects/CerebroNexus-backups/2026-10-13-builder-followup`。

| 检查 | 结果及适用代码范围 |
|---|---|
| 完整 Nix 源码测试 | `nix-final.log`：24,539 passed，0 failed/error，1 warning，13 skipped。固定快照包含后台事务等修复，但早于最后的回调体积、原生路径、安装测试路径和文案补丁。 |
| 修复后完整 Nix 源码测试 | `nix-source7.log`：24,556 passed，0 failed/error，1 warning，14 skipped。包含回调、原生路径、安装路径及保存提示修复；早于最后四段 Viewer 帮助文案。一个 IR 测试因并行 Chrome 调试端口冲突未启动，后续单独重跑及 source8 已通过；不回写本轮跳过结果。 |
| 后续完整 Nix 源码测试 | `nix-source8.log`：24,557 passed，0 failed/error，1 warning，13 skipped。Chrome 在加载测试固定随机种子前启动，避免并行进程使用相同调试端口；IR 首屏实际通过。包含四段帮助精简，后续轨迹帮助的语义修正包含在 check9。 |
| 安装后定向测试 | `installed-delta.log`：2,360 passed，0 failed/error/warning，1 skipped。测试只有复制的 tests 目录，实际读取安装包；涵盖 project、app-bundle、stage-contract、worker-app、async 等。 |
| 真实后台任务补充 | `test-background.R`：34 项断言通过；实际 callr 事务冲突/重试、图片附件核验、参数往返、配置复用、路径边界和精简回调。 |
| 最新源码定向测试 | `latest-targeted.log`：1,327 passed，0 failed/error/warning/skipped；覆盖后台写入、导入队列、数据页面、worker、Viewer 文案等。 |
| 真实导入与能力回传 | `import-final.log`：146 passed，0 failed/error/warning，3 skipped（QS reader）。 |
| 最后安装路径修复 | `installed-final.log`：486 passed，0 failed/error/warning，2 skipped；项目恢复、示例读取和实际 Canvas Node 行为测试。跳过项为 `.Rbuildignore` 明确排除的仓库生成器，不是安装后运行功能。 |
| Nix HDF5 ABI 回归 | `nix-hdf-load-order.log`：4 passed，无失败/警告/跳过。新进程中双向 namespace 加载并读写两个存储后端。 |
| 最后文案边界定向测试 | `final13.log`：246 passed，0 failed/error/warning/skipped。使用最终快照，覆盖平台能力、仅 RDS 可用或缺省显示名时的提示、导入队列、Data 页及点击行为。 |
| 最终代码安装后定向测试 | `installed13.log`：246 passed，0 failed/error/warning/skipped。先安装 final13 至独立库，另一个只有 tests 的目录运行上述定向测试，实际使用安装布局。 |
| 单独启用 IR/图片浏览器验收 | `nix-ir-retry.log` 中 IR 87 项、Builder 浏览器 145 项通过，没有跳过；包括此前端口冲突未启动的 IR 首屏和默认关闭的重复图片命名弹窗测试。同一批压力测试另有失败，不能把整个日志称为通过。 |
| 大文件导入压力测试 | `stress-metrics.log`：73 passed，0 failed/error/warning/skipped；8 × 约 64 MiB QS2，含无效文件后的继续导入、保留副本、快照恢复和单对象驻留。增长上限保持 384 MiB，增加导入期间的内存采样。 |
| 首次完整 Nix 包检查 | 含测试和 vignette，失败：1 ERROR、3 NOTEs；243 项失败主要是源码布局假设，另有身份验证、HDF5Array 加载和 Chrome 启动问题。保留失败日志，不以跳过测试代替。 |
| 修复后完整 Nix 包检查 | `nix-check7.log`：0 ERROR、0 WARNING、3 NOTEs；含测试、examples 和 vignette 重建。包内测试为 23,884 PASS、0 FAIL、1 WARN、92 SKIP；具体跳过原因保留在 `testthat.Rout`，不能说没有跳过单个测试。 |
| 后续完整 Nix 包检查 | `nix-check9.log`：0 ERROR、0 WARNING、3 NOTEs；含最终四段帮助修正。包内测试仍为 23,884 PASS、0 FAIL、1 WARN、92 SKIP；examples、安装加载和 vignette 重建均通过，退出码 0。完整明细另存 `nix-check9-testthat.Rout` 和 `nix-check9-00check.log`。 |
| 静态收尾 | 119 个修改/新增 R 文件均可解析；修改的 JavaScript 语法检查通过；两份 Viewer contract 字节一致；`git diff --check` 无空白错误。 |

第二轮和第四轮完整检查在用户更新/重启 WSL 时中断，没有最终结论；第三轮已主动停止，
因为被修复 HDF5 后的第四轮替代。保留所有失败/中断日志。
第五轮在依赖准备阶段被包含 WSL 测试隔离修正的第六轮替代，未执行包检查。
第六轮检查期间的 UI 复测发现保存成功文案不准确，修正后由第七轮替代。
第七轮包检查通过，包含所有功能修复，早于最后四段 Viewer 帮助精简。
第九轮使用包含最终帮助修正的固定快照重复完整包检查，同样以 0 ERROR、0 WARNING、
3 NOTEs 完成。
最后一处 CSS 焦点颜色修正单独经过真实浏览器验证；随后 reader 替代格式提示按实际能力
选择的补丁通过 245 项定向测试。两者位于 `source-followup-final10`，没有重启整套 R 测试。
`source-followup-final12` 另含压力测试的监测修正，运行时代码与 final10 相同。
最终 `source-followup-final13` 增加格式记录缺省显示名的兼容处理，源码与安装后各
246 项定向回归通过。上述完整检查与末尾定向检查的范围明确分开，不宣称后续小改动
重新运行了全部测试。

压力测试首次失败也保留：`nix-ir-retry.log` 在读取内存时误判进程不存在；
`ps-builder.log` 中 8 个 worker 实际存活，其中 7 个 processx 缓存句柄的创建时间与
ps 当前读数相差约 1.78 s；用 ps 自己捕获 PID 和创建时间后 8 个均能读取。
监测仍保留 PID 复用保护。
独立重跑 `stress-alone.log` 又发现原内存基线把懒加载依赖算入数据增长，超出原阈值。
现在先用一个很小的真实数据导入并释放以记录固定依赖开销，再测 8 个大文件，
**没有放宽原增长阈值**。最终冷启动 79.3 MiB，依赖就绪 606.8 MiB，导入采样峰值
789.9 MiB；增量约 183.1 MiB。该采样不等于连续、无遗漏的系统峰值，也不是大型真实
单细胞对象的内存上限。

两轮包检查的三个 NOTE 分别是既有未引用 Imports、`launchCerebro.R` 的全局配置赋值、
Chrome 临时目录残留。测试内的一个 warning 为 Seurat 将矩阵转换为 dgCMatrix；
R CMD check 自身没有 WARNING。安装测试跳过项包括源码目录专属契约、仓库生成脚本、
平台专属测试、缺失的可选 QS reader 及默认关闭的压力测试；源码全量与独立启用的
压力/浏览器测试另行记录，不相互替代。

这些是本地执行 GitHub 工作流对应的 Nix 命令，不是远端 GitHub CI 绿灯。
完整源码测试为 `devtools::load_all()` 后全量 `testthat::test_dir()`；完整包检查为
`devtools::check(vignettes = TRUE)`，保持测试开启，并以 ERROR/WARNING 作为失败条件。
没有使用 `--no-tests` 的包检查替代完整检查。

`final-code-sha256.json` 记录最终修改文件的 SHA-256。最终实现和测试文件与
`source-followup-final13` 逐字节核对一致；报告文本在测试完成后更新。
119 个修改/新增 R 文件同时通过解析与 Air 格式检查；JavaScript 语法和差异空白检查通过。
所有独立 UI 服务已停止；没有停止用户其他服务。

仍未覆盖：全部浏览器缩放倍率、系统 reduced-motion 偏好、所有平台原生文件夹框、
真实百万细胞数据、完整读屏器验收，以及生产负载延迟分位数。

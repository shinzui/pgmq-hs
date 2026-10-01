# Bundle Update Log

## 2026-10-01
* **Update**: BUG-1 fixed on the default branch: isTransient now classifies the empty-SQLSTATE and stray-result disconnect shapes as transient (ExecPlan 26)

## 2026-09-25
* **Addition**: Record unthrottled partition-channel notifications on partitioned queues.
* **Addition**: Record false creator reports during concurrent queue reconciliation.
* **Addition**: Record reproducible disconnect classification errors during PostgreSQL faults.
* **Bootstrap**: Establish the bug-report bundle under the shared `coordination.bugReports` profile.

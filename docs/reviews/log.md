# Bundle Update Log

## 2026-09-16
* **Addition**: REV-1 through REV-5 record the model review of pgmq-migration, pgmq-config, pgmq-hasql, pgmq-core, and pgmq-effectful over v0.4.0.1..v0.6.1.0 (163413b3); the partitioned-queue notification storm, the reconciler alias and drift findings, and the ephemeral-root permission remark produced MasterPlan 6 and plans 23–25.
* **Bootstrap**: Adopt the shared `assurance.reviews` profile from
`mori://shinzui/okf-profiles/profiles/reviews`, declare this directory as an OKF v0.2
bundle, and add strict profile and log validation.

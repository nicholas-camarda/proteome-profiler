test_that("active path and repository contracts use the workspace and Project Vault", {
    original_config <- proteome_profiler_config
    on.exit({
        proteome_profiler_config <<- original_config
    }, add = TRUE)

    runtime_root <- file.path(tempdir(), "proteome-runtime-contract")
    vault_root <- file.path(tempdir(), "Project Vault", "Research", "proteome-profiler")
    proteome_profiler_config <<- list(
        runtime_root = runtime_root,
        vault_root = vault_root,
        analyses = list()
    )

    candidates <- path_candidates(file.path("data", "raw", "example.csv"))
    expect_equal(candidates, c(
        file.path(repo_root, "data", "raw", "example.csv"),
        file.path(runtime_root, "data", "raw", "example.csv"),
        file.path(vault_root, "data", "raw", "example.csv")
    ))
    expect_false(any(grepl("VEGFRi and Dox|proteome_profiler", candidates)))
    expect_equal(
        resolve_project_path(file.path("output", "tester", "smoke"), must_exist = FALSE),
        normalizePath(file.path(runtime_root, "output", "tester", "smoke"), winslash = "/", mustWork = FALSE)
    )

    registry <- paste(readLines(file.path(repo_root, "analysis_registry.yaml"), warn = FALSE), collapse = "\n")
    expect_match(registry, "project_slug: proteome-profiler", fixed = TRUE)
    expect_match(registry, "repository: proteome-profiler", fixed = TRUE)
    expect_match(registry, "runtime_home: ~/Workspaces/proteome-profiler/runtime", fixed = TRUE)
    expect_match(registry, "Project Vault/Research/proteome-profiler", fixed = TRUE)
    expect_match(registry, "publish_root_name: outputs", fixed = TRUE)

    readme <- paste(readLines(file.path(repo_root, "README.md"), warn = FALSE), collapse = "\n")
    expect_match(readme, "https://github.com/nicholas-camarda/proteome-profiler.git", fixed = TRUE)
    expect_false(grepl("nicholas-camarda/proteome_profiler.git", readme, fixed = TRUE))
})

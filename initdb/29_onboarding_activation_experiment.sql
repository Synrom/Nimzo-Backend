-- Register capabilities without launching a new treatment allocation. Existing
-- sticky assignments always survive future operator weight changes.
INSERT INTO experiments (experiment_key,status,default_variant,respect_existing_assignments)
VALUES ('onboarding_activation_path_v1','active','control',TRUE)
ON CONFLICT (experiment_key) DO NOTHING;
INSERT INTO experiment_variants (experiment_key,variant_key,weight)
VALUES ('onboarding_activation_path_v1','control',100),
       ('onboarding_activation_path_v1','one_lesson',0)
ON CONFLICT (experiment_key,variant_key) DO NOTHING;

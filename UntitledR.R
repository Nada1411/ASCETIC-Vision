data(amlExample)
print(amlExample)
resExampleEvosigs <- evoSigs( survivalData = amlExample$survival_data,
                              evolutionarySteps = amlExample$evolutionary_steps )
